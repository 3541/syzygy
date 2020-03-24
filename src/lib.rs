#![cfg_attr(not(test), no_std)]
#![cfg_attr(test, allow(unused_imports))]
#![cfg_attr(test, allow(dead_code))]
#![feature(asm)]
#![feature(const_fn)]
#![feature(abi_x86_interrupt)]
#![feature(alloc_error_handler)]
#![feature(ptr_internals)]

mod arch;
mod constants;
mod hardware;
mod interrupt;
mod log;
mod memory;
mod panic;
mod vga_text;

extern crate alloc;
use logc::{debug, info};

use constants::KERNEL_BASE;
use memory::paging::table::ActiveTopLevelTable;
use memory::{Address, PhysicalAddress, VirtualAddress};

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

#[cfg(not(test))]
#[alloc_error_handler]
fn alloc_err(layout: alloc::alloc::Layout) -> ! {
    panic!("Allocator error!\n\t{:?}", layout);
}

#[cfg(not(feature = "integration-tests"))]
#[no_mangle]
pub extern "C" fn kmain(multiboot_info_addr: usize, _stack_bottom: usize) {
    vga_text::WRITER.lock().clear_screen();
    println!("ENTERED kmain");

    log::init();
    info!("INITIALIZED log");

    let multiboot_info_addr_phys = PhysicalAddress::new(multiboot_info_addr);
    let multiboot_info_addr = KERNEL_BASE + multiboot_info_addr;
    let multiboot_info = unsafe { multiboot2::load(*multiboot_info_addr) };
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
        .map(|s| VirtualAddress::new(s.start_address() as usize))
        .filter(|s| *s >= KERNEL_BASE)
        .min()
        .unwrap();
    let kernel_start_addr_phys = PhysicalAddress::new(kernel_start_addr - KERNEL_BASE);
    let kernel_end_addr = elf_sections
        .sections()
        .map(|s| VirtualAddress::new(s.end_address() as usize))
        .max()
        .unwrap();
    let kernel_end_addr_phys = PhysicalAddress::new(kernel_end_addr - KERNEL_BASE);

    debug!(
        "Kernel (VIRTUAL): {} - {}",
        kernel_start_addr, kernel_end_addr
    );

    debug!(
        "Kernel (PHYSICAL): {} - {}",
        kernel_start_addr_phys, kernel_end_addr_phys,
    );

    debug!(
        "Multiboot (VIRTUAL): {} - 0x{:x}",
        multiboot_info_addr,
        multiboot_info.end_address()
    );

    debug!(
        "Multiboot (PHYSICAL): {} - 0x{:x}",
        multiboot_info_addr_phys,
        multiboot_info.end_address() - *KERNEL_BASE
    );

    interrupt::init();
    info!("INITIALIZED interrupts");

    let mut allocator = memory::BitmapFrameAllocator::new(
        kernel_start_addr_phys,
        kernel_end_addr_phys,
        multiboot_info_addr_phys,
        PhysicalAddress::new(multiboot_info.end_address() - *KERNEL_BASE),
        mmap.memory_areas(),
        unsafe { &mut memory::bitmap_frame_allocator::BITMAP },
    );
    info!("INITIALIZED BitmapFrameAllocator");

    let mut table = unsafe { ActiveTopLevelTable::new() };
    info!("INITIALIZED PML4");

    memory::paging::remap_kernel(
        &mut allocator,
        &mut table,
        elf_sections.sections(),
        &multiboot_info,
    );
    info!("REMAPPED the kernel address space");

    memory::init_heap(&mut table, &mut allocator);
    info!("INITIALIZED kernel heap.");
}
