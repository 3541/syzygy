/*
 * MEM ARCH: Architecture-specific memory management.
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

use crate::mem::types::raw::RawVirtualAddress;

const NONCANONICAL_START: RawVirtualAddress = RawVirtualAddress(0x0000_8000_0000_0000);
const NONCANONICAL_END: RawVirtualAddress = RawVirtualAddress(0xFFFF_8000_0000_0000);

pub const fn is_valid(addr: RawVirtualAddress) -> bool {
    addr < NONCANONICAL_START || addr >= NONCANONICAL_END
}
