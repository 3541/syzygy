/*
 * MEM: Memory management.
 *
 * Copyright (c) 2026 Alex O'Brien <3541@3541.website>
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

mod arch;
mod heap;
mod phys;
mod types;
mod util;

use alloc::vec::Vec;

use common::{constants::MB, mmap::Mmap};
pub use heap::DefaultAlloc;
use log::debug;
pub use phys::PhysAlloc;
pub use types::{PhysicalAddress, VirtualAddress};
pub use util::{align_down, align_up};

pub fn init(mmap: Mmap) {
    phys::init(mmap);

    {
        let test_alloc = PhysAlloc::the()
            .alloc_contiguous(48 * MB)
            .expect("Failed to allocate");
        debug!("Test allocation: {test_alloc:#x?}.");
    }

    let mut allocs = Vec::new_in(DefaultAlloc::the());
    for alloc in PhysAlloc::the().try_alloc(48 * MB) {
        allocs.push(alloc);
    }

    debug!(
        "More test allocations: {allocs:#x?}. Total size: {} MB",
        allocs.iter().map(|a| a.size).sum::<usize>() / MB
    );
}
