//! x86_64 system initialization.

pub mod stivale2;

use log_crate::info;

use crate::boot;
use crate::consts::image::base;
use crate::int::arch as int;
use crate::io::log;
use crate::mem::arch::gdt;
use crate::mem::size;
use crate::util::register;
use stivale2::{StivaleHeader, StivaleInfo};

/// The initial stack, for use before memory management is enabled.
#[link_section = ".bss"]
static INIT_STACK: [u8; 32 * size::KB] = [0; 32 * size::KB];

/// The Stivale 2 header. See [the spec](https://github.com/stivale/stivale/blob/master/STIVALE2.md) for details.
#[link_section = ".stivale2hdr"]
#[used]
#[no_mangle]
static STIVALE_HEADER: StivaleHeader =
    unsafe { StivaleHeader::new(&INIT_STACK[INIT_STACK.len() - 16] as *const u8) };

/// Configure CPU features not already set up by the bootloader.
fn features_init() {
    // Enable NX.
    let efer = register::read::efer() | register::msr::EferFlags::NXE;
    unsafe { register::write::efer(efer) };
}

/// The actual kernel entry point. Performs architecture-specific initialization, then calls [kmain](boot::kmain).
#[no_mangle]
#[allow(unused)]
pub extern "C" fn kinit(info: &'static StivaleInfo) {
    features_init();

    log::init();

    info!("kinit.");
    info!("Booted by {}, version {}.", info.brand(), info.version());
    info!("Kernel loaded at {}.", base());

    gdt::init();
    int::init_early();

    info!("INITIALIZED IDT and GDT.");

    boot::kmain(info.slide() as usize, info.mmap());
}
