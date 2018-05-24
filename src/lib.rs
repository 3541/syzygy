#![feature(lang_items)]
#![feature(const_fn)]
#![feature(ptr_internals)]
#![no_std]

extern crate multiboot2;
extern crate spin;
extern crate volatile;

#[macro_use]
mod vga_buffer;
mod memory;

use memory::FrameAllocator;

#[no_mangle]
pub extern "C" fn syzygy_main(mb_info_addr: usize) {
    vga_buffer::clear_screen();

    println!("SYZYGY OK");

    let boot_info = unsafe { multiboot2::load(mb_info_addr) };
    let mmap_tag = boot_info
        .memory_map_tag()
        .expect("Memory map tag is missing!");

    println!("Memory regions:");
    for area in mmap_tag.memory_areas() {
        println!(
            "  Begins: 0x{:x}, size: 0x{:x}",
            area.start_address(),
            area.size()
        );
    }

    let elf_sections = boot_info
        .elf_sections_tag()
        .expect("ELF sections tag is missing!");
    println!("Kernel sections:");
    for section in elf_sections.sections() {
        println!(
            "  Begins: 0x{:x}, size: 0x{:x}, flags: 0x{:x}",
            section.start_address(),
            section.size(),
            section.flags()
        );
    }

    let kernel_start = elf_sections
        .sections()
        .map(|s| s.start_address())
        .min()
        .unwrap() as usize;
    let kernel_end = elf_sections
        .sections()
        .map(|s| s.end_address())
        .max()
        .unwrap() as usize;

    let multiboot_start = mb_info_addr as usize;
    let multiboot_end = multiboot_start + (boot_info.total_size() as usize);

    println!(
        "Kernel begins: 0x{:x}, ends: 0x{:x}",
        kernel_start, kernel_end
    );
    println!(
        "Multiboot begins: 0x{:x}, ends: 0x{:x}",
        multiboot_start, multiboot_end
    );

    let mut alloc = memory::SequentialFrameAllocator::new(
        kernel_start,
        kernel_end,
        multiboot_start,
        multiboot_end,
        mmap_tag.memory_areas(),
    );

    for i in 0.. {
        if let None = alloc.alloc_frame() {
            println!("Allocated {} frames", i);
            break;
        }
    }
    loop {}
}

#[lang = "eh_personality"]
#[no_mangle]
pub extern "C" fn eh_personality() {}

#[lang = "panic_fmt"]
#[no_mangle]
pub extern "C" fn panic_fmt(fmt: core::fmt::Arguments, file: &'static str, line: u32) -> ! {
    println!("SYZYGY PANIC");
    println!("  In: {} at line {}", file, line);
    println!("  {}", fmt);
    loop {}
}
