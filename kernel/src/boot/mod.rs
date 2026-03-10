/*
 * BOOT: Architecture-independent bootstrap.
 *
 * Copyright (c) 2024, 2026 Alex O'Brien <3541@3541.website>
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

use alloc::vec::Vec;
use log::info;

use crate::mem::DefaultAlloc;

fn kmain() {
    crate::mem::init();

    let mut v = Vec::new_in(DefaultAlloc::the());
    v.push(1);
    v.push(2);
    v.push(3);
    info!("Allocation: {v:?}");

    todo!("kmain");
}
