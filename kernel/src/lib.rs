#![no_std]
#![feature(const_fn)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(llvm_asm)]
#![feature(asm)]
#![feature(abi_x86_interrupt)]
#![feature(const_fn_fn_ptr_basics)]

/*
   Modules should:
       - Be architecture-independent.
       - Export a single top-level function init, which should be the only thing
         needed to initialize all submodules.
*/
#[macro_use]
mod io;

mod boot;
mod consts;
mod err;
mod int;
mod mem;
mod util;
