/*
 * SYZYGY: Kernel.
 *
 * Copyright (c) 2024, 2026 Alex O'Brien <3541@3541.website>
 *
 * This file is part of Syzygy.
 *
 * Syzygy is free software: you can redistribute it and/or modify it under the
 * terms of version 3 the GNU General Public License as published by the Free
 * Software Foundation.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software. If not, see <https://www.gnu.org/licenses/>.
 */

#![cfg_attr(not(test), no_std)]

#![feature(const_trait_impl)]
#![feature(derive_const)]
#![feature(const_clone)]
#![feature(const_cmp)]
#![feature(allocator_api)]
#![feature(ptr_as_ref_unchecked)]
#![feature(btreemap_alloc)]
#![feature(btree_cursors)]

extern crate alloc;

mod arch;
mod boot;
#[macro_use]
mod io;
mod mem;
mod sync;
mod util;

use log::error;

#[cfg_attr(not(test), panic_handler)]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    error!("PANIC: {}", info);
    loop {}
}
