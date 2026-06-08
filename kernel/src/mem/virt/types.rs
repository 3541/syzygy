/*
 * VIRT TYPES: Virtual memory management types.
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

use super::VirtualMapping;
use crate::mem::{VirtualAddress, virt::VMAlloc};

#[derive(Debug, Copy, Clone)]
pub struct VirtualArea {
    pub start: VirtualAddress,
    pub size: usize,
}

impl VirtualArea {
    pub fn end(&self) -> VirtualAddress {
        self.start + self.size
    }

    pub fn contains_addr(&self, addr: VirtualAddress) -> bool {
        self.start <= addr && addr < self.end()
    }

    pub fn contains_area(&self, other: VirtualArea) -> bool {
        self.contains_addr(other.start) && other.end() <= self.end()
    }
}

#[derive(Debug)]
pub enum VirtualAllocation {
    Unmapped(VirtualArea),
    Mapped(VirtualMapping),
}

impl VirtualAllocation {
    pub(super) unsafe fn unmapped(area: VirtualArea) -> Self {
        Self::Unmapped(area)
    }
}

impl Deref for VirtualAllocation {
    type Target = VirtualArea;

    fn deref(&self) -> &VirtualArea {
        match self {
            Self::Unmapped(area) => area,
            Self::Mapped(mapping) => mapping.area(),
        }
    }
}

impl Drop for VirtualAllocation {
    fn drop(&mut self) {
        if let Self::Mapped(mapping) = self {
            todo!("Unmap area and potentially free backing.");
        }

        // TODO: Handle non-kernel allocations.
        // SAFETY: There cannot be any repeated calls to drop after this.
        unsafe { VMAlloc::kernel().free(self) }
    }
}
