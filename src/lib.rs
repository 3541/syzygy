#![no_std]
//#![feature(ptr_internals)]
//#![feature(lang_items)]

extern crate spin;
extern crate volatile;
extern crate lazy_static;

mod vga_text;

#[no_mangle]
pub extern "C" fn kmain() {
    /*    let message = b"[ENTER] kmain";
    let color = 0x0f;

    let mut message_colored = [color; 26];
    for (i, b) in message.into_iter().enumerate() {
        message_colored[i * 2] = *b;
    }

    let vga_buf = (0xC00B8000) as *mut _;
    unsafe { *vga_buf = message_colored };*/

    use core::fmt::Write;
    vga_text::WRITER.lock().write_str("[ENTER] kmain").unwrap();
//    loop {}
}

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
