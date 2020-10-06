mod stivale2;

use crate::boot;
use crate::int::arch as int;
use crate::mem::arch::gdt;
use stivale2::StivaleHeader;

#[link_section = ".bss"]
static INIT_STACK: [u8; 32768] = [0; 32768];

#[link_section = ".stivale2hdr"]
#[used]
#[no_mangle]
static STIVALE_HEADER: StivaleHeader =
    unsafe { StivaleHeader::new(&INIT_STACK[INIT_STACK.len() - 16] as *const u8, false) };

#[no_mangle]
#[allow(unused)]
pub extern "C" fn kinit() {
    gdt::init();
    int::init_idt();

    boot::kmain();
}
