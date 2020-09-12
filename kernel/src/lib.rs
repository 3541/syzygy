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

mod consts;
mod err;
mod int;
mod log;
mod mem;
mod util;

#[no_mangle]
#[allow(unused)]
pub extern "C" fn kmain(_multiboot_info_address: usize) {
    log::init();

    loop {}
}
