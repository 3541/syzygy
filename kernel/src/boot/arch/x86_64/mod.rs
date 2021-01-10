pub mod stivale2;

use log_crate::info;

use crate::boot;
use crate::int::arch as int;
use crate::io::log;
use crate::mem::arch::gdt;
use crate::mem::size;
use crate::util::register;
use stivale2::{StivaleHeader, StivaleInfo};

#[link_section = ".bss"]
static INIT_STACK: [u8; 32 * size::KB] = [0; 32 * size::KB];

#[link_section = ".stivale2hdr"]
#[used]
#[no_mangle]
static STIVALE_HEADER: StivaleHeader =
    unsafe { StivaleHeader::new(&INIT_STACK[INIT_STACK.len() - 16] as *const u8, false) };

// Configure CPU features not already set up by the bootloader.
fn features_init() {
    // Enable NX.
    let efer = register::read::efer() | register::msr::EferFlags::NXE;
    unsafe { register::write::efer(efer) };
}

#[no_mangle]
#[allow(unused)]
pub extern "C" fn kinit(info: &'static StivaleInfo) {
    features_init();

    log::init();

    info!("kinit.");
    info!("Booted by {}, version {}.", info.brand(), info.version());

    gdt::init();
    int::init_idt();

    info!("INITIALIZED IDT and GDT.");

    boot::kmain(info.mmap());
}
