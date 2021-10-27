#![cfg_attr(not(test), no_std)]
#![feature(llvm_asm)]
#![feature(asm)]
#![feature(abi_x86_interrupt)]
#![allow(incomplete_features)]
#![feature(const_trait_impl)]
#![feature(alloc_error_handler)]
#![feature(const_mut_refs)]
#![feature(const_panic)]
#![feature(const_fn_transmute)]
#![feature(const_fn_fn_ptr_basics)]
#![feature(inherent_associated_types)]
#![feature(thread_local)]
#![feature(adt_const_params)]
#![feature(generic_const_exprs)]
#![cfg_attr(test, allow(unused))]

/*!

The Syzygy kernel.

Modules should:
- Be architecture-independent.
- Export a single top-level function `init`, which should be the only thing needed to initialize all submodules.

!*/

extern crate alloc;

#[macro_use]
mod arch;
#[macro_use]
mod util;
#[macro_use]
mod io;

mod boot;
mod consts;
mod err;
mod int;
mod mem;
mod task;
