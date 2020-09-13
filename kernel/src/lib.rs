#![no_std]
#![feature(const_fn)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(llvm_asm)]
#![feature(asm)]

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

#[no_mangle]
#[allow(unused)]
pub extern "C" fn kmain(_multiboot_info_address: usize) {
    log::init();

    println!("-> kmain");

    unsafe { asm!("hlt") };
}
