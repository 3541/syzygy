/*
 * RAW MEM TYPES: Primitive memory management types without validation.
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

use core::{fmt, ops::{Add, Sub}};
use crate::mem::align_up;

#[derive_const(Clone, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Debug)]
pub struct RawPhysicalAddress(pub usize);

impl Copy for RawPhysicalAddress {}

impl fmt::Display for RawPhysicalAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "P{:#x}", self.0)
    }
}

#[derive_const(Clone, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Debug)]
pub struct RawVirtualAddress(pub usize);

impl RawVirtualAddress {
    pub const fn align_up(&self, align: usize) -> Self {
        Self(align_up(self.0, align))
    }
}

impl Copy for RawVirtualAddress {}

impl fmt::Display for RawVirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "V{:#x}", self.0)
    }
}

impl<T> From<*mut T> for RawVirtualAddress {
    fn from(ptr: *mut T) -> Self {
        Self(ptr as usize)
    }
}

impl<T> From<*const T> for RawVirtualAddress {
    fn from(ptr: *const T) -> Self {
        Self(ptr as usize)
    }
}

impl Add<usize> for RawVirtualAddress {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        Self(self.0.checked_add(rhs).expect("Overflowed virtual address addition."))
    }
}

impl Sub<RawVirtualAddress> for RawVirtualAddress {
    type Output = isize;

    fn sub(self, rhs: RawVirtualAddress) -> isize {
        (self.0 as isize) - (rhs.0 as isize)
    }
}
