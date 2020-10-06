pub mod stivale2;

use log_crate::info;

use crate::boot;
use crate::int::arch as int;
use crate::io::log;
use crate::mem::arch::gdt;
use stivale2::{StivaleHeader, StivaleInfo};

#[link_section = ".bss"]
static INIT_STACK: [u8; 32768] = [0; 32768];

#[link_section = ".stivale2hdr"]
#[used]
#[no_mangle]
static STIVALE_HEADER: StivaleHeader =
    unsafe { StivaleHeader::new(&INIT_STACK[INIT_STACK.len() - 16] as *const u8, false) };

#[no_mangle]
#[allow(unused)]
pub extern "C" fn kinit(info: &'static StivaleInfo) {
    log::init();

    info!("kinit.");
    info!("Booted by {}, version {}.", info.brand(), info.version());

    gdt::init();
    int::init_idt();

    boot::kmain(info.mmap());
}
