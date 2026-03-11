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

use crate::mem::PhysicalAddress;

#[derive(Debug, Copy, Clone)]
pub struct PhysicalArea {
    pub start: PhysicalAddress,
    pub size: usize,
}

impl PhysicalArea {
    pub fn page_count(&self) -> usize {
        self.size / self.page_size()
    }
}

pub struct PhysicalAllocation(PhysicalArea);

impl Drop for PhysicalAllocation {
    fn drop(&mut self) {
        todo!()
    }
}
