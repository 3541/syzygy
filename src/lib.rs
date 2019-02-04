#![cfg_attr(not(test), no_std)]

extern crate spin;
extern crate volatile;
extern crate lazy_static;

mod vga_text;

#[cfg(target_arch = "x86")]
const KERNEL_BASE: usize = 0xC0000000;

#[cfg(target_arch = "x86_64")]
const KERNEL_BASE: usize = 0xFFFFC00000000000;

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

    vga_text::WRITER.lock().clear_screen();
    println!("[ENTER] kmain");
//    loop {}
}

#[panic_handler]
#[cfg(not(test))]
fn panic(info: &core::panic::PanicInfo) -> ! {
    println!("{}", info);
    loop {}
}
