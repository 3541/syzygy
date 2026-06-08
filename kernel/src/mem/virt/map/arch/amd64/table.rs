/*
 * VM MAP: amd64 page tables.
 *
 * Copyright (c) 2021-2022, 2026 Alex O'Brien <3541@3541.website>
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

use bitflags::bitflags;

bitflags! {
    pub struct EntryFlags: usize {
        const PRESENT = 1;
        const WRITABLE = 1 << 1;
        const USER_ACCESSIBLE = 1 << 2;
        const WRITE_THROUGH = 1 << 3;
        const CACHE_DISABLED = 1 << 4;
        const ACCESSED = 1 << 5;
        const DIRTY = 1 << 6;
        const LARGE = 1 << 7;
        const GLOBAL = 1 << 8;
        const NO_EXECUTE = 1 << 63;
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct Entry(usize);

impl Entry {
    const ADDRESS_MASK: usize = 0x000F_FFFF_FFFF_F000;
}

struct Num<const N: usize>;
trait GreaterThanZero {}
impl<const N: usize> GreaterThanZero for Num<N> where [(); N - 1]: Sized {}

// L0: Page
// L1: PT
// L2: PD
// L3: PDP
// L4: PML4
#[repr(transparent)]
pub struct PageTable<const LEVEL: usize>([Entry; 512])
where
    Num<LEVEL>: GreaterThanZero;

impl<const LEVEL: usize> PageTable<LEVEL> where Num<LEVEL>: GreaterThanZero {}
