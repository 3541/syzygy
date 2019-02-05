#![cfg_attr(not(test), no_std)]
#![feature(asm)]
#![feature(const_fn)]

#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;
extern crate spin;
extern crate volatile;

mod hardware;
mod vga_text;

#[cfg(target_arch = "x86")]
const KERNEL_BASE: usize = 0xC0000000;

#[cfg(target_arch = "x86_64")]
const KERNEL_BASE: usize = 0xFFFFC00000000000;

#[no_mangle]
pub extern "C" fn kmain() {
    vga_text::WRITER.lock().clear_screen();
    println!("kmain start");
    serial_println!("kmain start");
    //    loop {}
}

#[panic_handler]
#[cfg(not(test))]
fn panic(info: &core::panic::PanicInfo) -> ! {
    println!("{}", info);
    loop {}
}
