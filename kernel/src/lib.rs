#![no_std]
#![feature(const_fn)]
#![feature(llvm_asm)]
#![feature(asm)]
#![feature(abi_x86_interrupt)]
#![feature(min_const_generics)]
#![allow(incomplete_features)]
#![feature(const_trait_impl)]

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
