#![no_std]

mod boot;
#[macro_use]
mod io;

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
