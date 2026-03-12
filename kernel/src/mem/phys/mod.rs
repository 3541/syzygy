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

use log::trace;

use common::mmap::Mmap;

use bitmap::BitmapAlloc;
pub use types::{PhysicalAllocation, PhysicalArea};

pub type PhysAlloc = BitmapAlloc;

pub(super) fn init(mmap: Mmap) {
    trace!(
        "Initializing physical memory management. Memory map: {:#x?}",
        mmap.map
    );

    bitmap::init(mmap);
}
