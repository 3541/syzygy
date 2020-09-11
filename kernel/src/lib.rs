#![no_std]

mod log;

#[no_mangle]
pub extern "C" fn kmain(_multiboot_info_address: usize) {
    loop {}
}
