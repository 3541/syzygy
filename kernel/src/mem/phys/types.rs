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

use core::ops::Deref;

use crate::mem::{PhysicalAddress, PhysAlloc};

#[derive(Debug, Copy, Clone)]
pub struct PhysicalArea {
    pub start: PhysicalAddress,
    pub size: usize,
}

#[derive(Debug)]
pub struct PhysicalAllocation(PhysicalArea);

impl PhysicalAllocation {
    // SAFETY: This has a Drop implementation which will hand back to the bitmap allocator, so it must actually come from the right place.
    pub(in crate::mem::phys) unsafe fn new(area: PhysicalArea) -> Self {
        Self(area)
    }
}

impl Deref for PhysicalAllocation {
    type Target = PhysicalArea;

    fn deref(&self) -> &PhysicalArea {
        &self.0
    }
}

impl Drop for PhysicalAllocation {
    fn drop(&mut self) {
        // SAFETY: This is the Drop implementation, so nothing will touch this afterwards.
        unsafe { PhysAlloc::the().free(self) }
    }
}
