/*
 * SYZYGY: Kernel.
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

#![no_std]

mod arch;
mod boot;
#[macro_use]
mod io;

use log::error;

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    error!("PANIC: {}", info);
    loop {}
}
