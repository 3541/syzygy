#![cfg_attr(not(test), no_std)]
#![cfg_attr(test, allow(unused_imports))]
#![cfg_attr(test, allow(dead_code))]
#![feature(asm)]
#![feature(const_fn)]
#![feature(abi_x86_interrupt)]
//#![feature(alloc_error_handler)]

//extern crate alloc;

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

#[cfg(target_arch = "x86")]
const KERNEL_BASE: usize = 0xC0000000;

#[cfg(target_arch = "x86_64")]
const KERNEL_BASE: usize = 0xFFFFC00000000000;

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

/*#[alloc_error_handler]
fn oom(_l: Layout) -> ! {
    panic!("Out of memory!");
}*/

#[panic_handler]
#[cfg(not(test))]
fn panic(info: &core::panic::PanicInfo) -> ! {
    println!("{}", info);
    loop {}
}

#[cfg(not(feature = "integration-tests"))]
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

    /*let mut allocator = memory::WatermarkFrameAllocator::new(
        kernel_start_addr as usize,
        kernel_end_addr as usize,
        multiboot_info_addr as usize,
        multiboot_info.end_address() as usize,
        mmap.memory_areas(),
    );*/

    /*    let mem = allocator
        .alloc(memory::FrameSize::Large)
        .expect("Failed to allocate.");
    println!("0x{:x} - 0x{:x}", mem.address(), mem.end_address());*/

    /*    for i in 0.. {
        if let None = allocator.alloc(memory::FrameSize::Large) {
            println!("Allocated {} frames", i);
            break;
        }
    }*/

    /*    unsafe {
        println!(
            "{:x?}",
            (*memory::paging::table::TOP_LEVEL_TABLE)[memory::paging::table::KERNEL_INDEX]
                .address()
        )
    };*/

    /*    let test_addr = KERNEL_BASE + 0x400;
    println!(
        "Test address translation 0x{:x} -> 0x{:x}",
        test_addr,
        memory::paging::translate(test_addr)
    );

    let mut s = hardware::serial::SERIAL1.lock();
    loop {
        match s.recv_byte() {
            Some(b) => vga_println!("received: {}", b),
            None => {}
        }
    }*/

    log::init();
    warn!("test warn");
    error!("test error");
    hardware::interrupt::init();

    unsafe {
        asm!("ud2" :::: "volatile");
    }
    //    let v = Box::new(2);

    //    unsafe { *(0xdeadffff as *mut u64) = 0 };
}
