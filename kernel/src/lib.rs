#![no_std]

#[macro_use]
mod arch;
mod boot;

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
