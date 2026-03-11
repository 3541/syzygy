/*
 * PHYS MEM: Physical memory management.
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
mod bitmap;
mod types;

use log::{info, trace};

use common::{constants::MB, mmap::Mmap};

use bitmap::BitmapAlloc;
pub use types::PhysicalArea;

pub type PhysAlloc = BitmapAlloc;

pub(super) fn init(mmap: Mmap) -> PhysAlloc {
    trace!(
        "Initializing physical memory management. Memory map: {:#x?}",
        mmap.map
    );

    let overall_size = mmap.max_usable_range.end - mmap.max_usable_range.start;
    let res = BitmapAlloc::new(mmap);
    let free = res.free_size();
    assert!(free < overall_size);
    info!(
        "Initialized physical memory allocator. {}/{} MB usable.",
        free / MB,
        overall_size / MB
    );

    res
}
