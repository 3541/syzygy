#![no_std]
#![feature(const_fn)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(llvm_asm)]
#![feature(asm)]
#![feature(abi_x86_interrupt)]

/*
   Modules should:
       - Be architecture-independent.
       - Export a single top-level function init, which should be the only thing
         needed to initialize all submodules.
*/
#[macro_use]
mod io;

mod consts;
mod err;
mod int;
mod mem;
mod util;

use io::log;

use log_crate::info;

#[no_mangle]
#[allow(unused)]
pub extern "C" fn kmain(_multiboot_info_address: usize) {
    log::init();
    info!("kmain.");

    info!("This is {}, version {}.", consts::NAME, consts::VERSION);

    int::init();
    info!("INITIALIZED interrupt handlers.");

    util::halt_loop();
}
