/*
 * PORT: AMD64 IO port access.
 *
 * Copyright (c) 2024 Alex O'Brien <3541@3541.website>
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

use core::{arch::asm, marker::PhantomData};

/// An IO port of a given integral size.
pub struct Port<S> {
    addr: u16,
    size: PhantomData<S>,
}

impl<S> Port<S> {
    pub const fn new(addr: u16) -> Self {
        Self {
            addr,
            size: PhantomData,
        }
    }
}

impl Port<u8> {
    /// Write a byte to the port.
    /// # Safety
    /// Depends entirely on the particular port.
    #[inline]
    pub unsafe fn write(&self, v: u8) {
        unsafe { asm!("out dx, al", in("dx") self.addr, in("al") v, options(nostack, nomem)) }
    }
}
