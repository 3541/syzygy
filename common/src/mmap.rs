/*
 * MMAP: Initial memory map.
 *
 * Copyright (c) 2020-2021, 2026 Alex O'Brien <3541@3541.website>
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

use core::ops::Range;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MmapEntryType {
    Usable,
    Reserved,
    Kernel,
    ACPIReclaimable,
}

#[derive(Debug)]
pub struct MmapEntry {
    pub entry_type: MmapEntryType,
    pub start_phys: usize,
    pub size: usize,
}

impl MmapEntry {
    pub fn end_phys(&self) -> usize {
        self.start_phys + self.size
    }
}

pub struct Mmap {
    pub map: &'static [MmapEntry],
    pub max_usable_range: Range<usize>,
}
