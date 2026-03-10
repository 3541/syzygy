/*
 * MEM TYPES: Memory management types.
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

pub(super) mod raw;

use core::ops::{Add, Sub};

use super::arch::is_valid;
pub use raw::{RawPhysicalAddress, RawVirtualAddress};

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct PhysicalAddress(RawPhysicalAddress);

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct VirtualAddress(RawVirtualAddress);

impl VirtualAddress {
    pub const fn new(raw: RawVirtualAddress) -> Self {
        assert!(is_valid(raw));
        Self(raw)
    }

    pub const unsafe fn new_unchecked(raw: RawVirtualAddress) -> Self {
        Self(raw)
    }

    pub const fn is_valid(raw: RawVirtualAddress) -> bool {
        is_valid(raw)
    }

    pub fn of<T>(r: &T) -> Self {
        Self::from(r as *const _)
    }

    pub const fn align_up(&self, align: usize) -> Self {
        Self::new(self.0.align_up(align))
    }

    pub fn as_mut_ptr<T>(&self) -> *mut T {
        self.0.0 as *mut T
    }
}

impl<T> From<*mut T> for VirtualAddress {
    fn from(ptr: *mut T) -> Self {
        let addr = RawVirtualAddress::from(ptr);
        Self::new(addr)
    }
}

impl<T> From<*const T> for VirtualAddress {
    fn from(ptr: *const T) -> Self {
        let addr = RawVirtualAddress::from(ptr);
        Self::new(addr)
    }
}

impl Add<usize> for VirtualAddress {
    type Output = VirtualAddress;

    fn add(self, rhs: usize) -> Self {
        Self::new(self.0 + rhs)
    }
}

impl Sub<VirtualAddress> for VirtualAddress {
    type Output = <RawVirtualAddress as Sub<RawVirtualAddress>>::Output;

    fn sub(self, rhs: VirtualAddress) -> Self::Output {
        self.0 - rhs.0
    }
}
