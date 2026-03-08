/*
 * INIT: amd64 bootstrap.
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

use core::arch::asm;

use log::info;

use crate::boot::kmain;

#[unsafe(no_mangle)]
extern "C" fn kinit() {
    unsafe { asm!("out dx, al", in("dx") 0xe9, in("al") b'Y', options(nostack, nomem) )};
    // info!("Syzygy kernel amd64 {}.", env!("SZ_VER"));

    crate::io::log::init();
    kmain();
}
