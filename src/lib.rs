#![cfg_attr(not(test), no_std)]
#![feature(asm)]
#![feature(const_fn)]

#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;
extern crate multiboot2;
extern crate spin;
extern crate volatile;

mod hardware;
mod memory;
mod vga_text;

use crate::memory::FrameAllocator;

#[cfg(target_arch = "x86")]
const KERNEL_BASE: usize = 0xC0000000;

#[cfg(target_arch = "x86_64")]
const KERNEL_BASE: usize = 0xFFFFC00000000000;

#[no_mangle]
pub extern "C" fn kmain(multiboot_info_addr: usize) {
    vga_text::WRITER.lock().clear_screen();
    println!("kmain start");

    let multiboot_info_addr = multiboot_info_addr + KERNEL_BASE;
    let multiboot_info = unsafe { multiboot2::load(multiboot_info_addr) };
    let mmap = multiboot_info
        .memory_map_tag()
        .expect("Memory map tag is malformed/missing.");

    println!("Memory areas (PHYSICAL)");

    for a in mmap.memory_areas() {
        println!(
            "\t0x{:x} - 0x{:x} (0x{:x})",
            a.start_address(),
            a.end_address(),
            a.size()
        );
    }

    let elf_sections = multiboot_info
        .elf_sections_tag()
        .expect("ELF sections tag is malformed/missing.");

    println!("Kernel sections (FUCKED):");
    for s in elf_sections.sections() {
        println!(
            "\t0x{:x} - 0x{:x} (0x{:x}) -- FLAGS: 0x{:x}",
            s.start_address(),
            s.end_address(),
            s.size(),
            s.flags()
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

    println!(
        "Kernel (VIRTUAL): 0x{:x} - 0x{:x}",
        kernel_start_addr, kernel_end_addr
    );

    println!(
        "Kernel (PHYSICAL): 0x{:x} - 0x{:x}",
        kernel_start_addr as usize - KERNEL_BASE,
        kernel_end_addr as usize - KERNEL_BASE
    );

    println!(
        "Multiboot (VIRTUAL): 0x{:x} - 0x{:x}",
        multiboot_info_addr,
        multiboot_info.end_address()
    );

    println!(
        "Multiboot (PHYSICAL): 0x{:x} - 0x{:x}",
        multiboot_info_addr - KERNEL_BASE,
        multiboot_info.end_address() - KERNEL_BASE
    );

    println!("Test frame alloc");
    let mut allocator = memory::WatermarkFrameAllocator::new(
        kernel_start_addr as usize,
        kernel_end_addr as usize,
        multiboot_info_addr as usize,
        multiboot_info.end_address() as usize,
        mmap.memory_areas(),
    );

    /*    let mem = allocator
        .alloc(memory::FrameSize::Large)
        .expect("Failed to allocate.");
    println!("0x{:x} - 0x{:x}", mem.address(), mem.end_address());*/

    for i in 0.. {
        if let None = allocator.alloc(memory::FrameSize::Huge) {
            println!("Allocated {} frames", i);
            break;
        }
    }

    //    loop {}
}

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
unsafe fn exit_qemu() {
    hardware::Port::<u8>::new(0xF4).write(0);
}

#[panic_handler]
#[cfg(not(test))]
fn panic(info: &core::panic::PanicInfo) -> ! {
    println!("{}", info);
    loop {}
}
