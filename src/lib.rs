#![cfg_attr(not(test), no_std)]
#![cfg_attr(test, allow(unused_imports))]
#![cfg_attr(test, allow(dead_code))]
#![feature(asm)]
#![feature(const_fn)]
#![feature(abi_x86_interrupt)]
#![feature(alloc_error_handler)]
#![feature(ptr_internals)]

extern crate alloc;

#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;
extern crate multiboot2;
extern crate spin;
extern crate volatile;
#[macro_use]
extern crate logc;
extern crate ansi_rgb;
extern crate rgb;

mod arch;
mod constants;
mod hardware;
mod log;
mod memory;
mod vga_text;

use constants::KERNEL_BASE;
use memory::paging::table::ActiveTopLevelTable;
use memory::FrameAllocator;

#[cfg(feature = "integration-tests")]
#[no_mangle]
pub extern "C" fn kmain(multiboot_info_addr: usize) {
    integration_tests::run();
}

#[cfg(feature = "integration-tests")]
mod integration_tests;

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => {
        $crate::_print(format_args!($($arg)*));
    };
}

#[doc(hidden)]
pub fn _print(args: core::fmt::Arguments) {
    vga_text::_print(args);
    hardware::serial::_print(args);
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!('\n'));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[allow(dead_code)]
fn exit_qemu(code: u8) -> ! {
    if code == 0 {
        unsafe {
            hardware::Port::<u32>::new(0x604).write(0x2000);
        }
    } else {
        unsafe {
            hardware::Port::<u8>::new(0xF4).write(code >> 1);
        }
    }
    // NOTE: meaningless, makes ! work.
    loop {}
}

#[alloc_error_handler]
fn alloc_err(layout: alloc::alloc::Layout) -> ! {
    panic!("Allocator error!\n\t{:?}", layout);
}

#[panic_handler]
#[cfg(not(test))]
fn panic(info: &core::panic::PanicInfo) -> ! {
    error!("{}", info);
    loop {}
}

#[cfg(not(feature = "integration-tests"))]
#[no_mangle]
pub extern "C" fn kmain(multiboot_info_addr: usize, _stack_bottom: usize) {
    vga_text::WRITER.lock().clear_screen();
    println!("ENTERED kmain");
    log::init();
    info!("INITIALIZED log");

    let multiboot_info_addr = multiboot_info_addr + KERNEL_BASE;
    let multiboot_info = unsafe { multiboot2::load(multiboot_info_addr) };
    let mmap = multiboot_info
        .memory_map_tag()
        .expect("Memory map tag is malformed/missing.");

    info!("INITIALIZED memory map");

    debug!("Memory areas (PHYSICAL)");

    for a in mmap.memory_areas() {
        debug!(
            "\t0x{:x} - 0x{:x} (0x{:x})",
            a.start_address(),
            a.end_address(),
            a.size()
        );
    }

    let elf_sections = multiboot_info
        .elf_sections_tag()
        .expect("ELF sections tag is malformed/missing.");

    debug!("Kernel sections (FUCKED):");
    for s in elf_sections.sections() {
        debug!(
            "\t0x{:x} - 0x{:x} (0x{:x}) -- FLAGS: 0x{:x} (ALLOCATED: {})",
            s.start_address(),
            s.end_address(),
            s.size(),
            s.flags(),
            s.is_allocated(),
        );
    }

    let kernel_start_addr = elf_sections
        .sections()
        .map(|s| s.start_address())
        .filter(|s| *s as usize >= KERNEL_BASE)
        .min()
        .unwrap();
    let kernel_end_addr = elf_sections
        .sections()
        .map(|s| s.end_address())
        .max()
        .unwrap();

    debug!(
        "Kernel (VIRTUAL): 0x{:x} - 0x{:x}",
        kernel_start_addr, kernel_end_addr
    );

    debug!(
        "Kernel (PHYSICAL): 0x{:x} - 0x{:x}",
        kernel_start_addr as usize - KERNEL_BASE,
        kernel_end_addr as usize - KERNEL_BASE
    );

    debug!(
        "Multiboot (VIRTUAL): 0x{:x} - 0x{:x}",
        multiboot_info_addr,
        multiboot_info.end_address()
    );

    debug!(
        "Multiboot (PHYSICAL): 0x{:x} - 0x{:x}",
        multiboot_info_addr - KERNEL_BASE,
        multiboot_info.end_address() - KERNEL_BASE
    );

    /*    let mut allocator = memory::WatermarkFrameAllocator::new(
        kernel_start_addr as usize,
        kernel_end_addr as usize,
        multiboot_info_addr as usize,
        multiboot_info.end_address() as usize,
        mmap.memory_areas(),
    );
    info!("INITIALIZED WatermarkFrameAllocator");*/

    let mut allocator = memory::BitmapFrameAllocator::new(
        kernel_start_addr as usize,
        kernel_end_addr as usize,
        multiboot_info_addr as usize,
        multiboot_info.end_address() as usize,
        mmap.memory_areas(),
        unsafe { &mut memory::bitmap_frame_allocator::BITMAP },
    );
    info!("INITIALIZED BitmapFrameAllocator");

    hardware::interrupt::init();
    info!("INITIALIZED interrupts");

    let mut table = unsafe { ActiveTopLevelTable::new() };
    info!("INITIALIZED top-level page table");

    memory::paging::remap_kernel(
        &mut allocator,
        &mut table,
        elf_sections.sections(),
        &multiboot_info,
    );
    allocator.alloc();
    info!("REMAPPED the kernel address space");
}
