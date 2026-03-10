/*
 * SYNC INTERRUPTS: Interrupt utilities.
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

#[cfg(not(test))]
use core::arch::asm;

#[inline]
#[cfg(not(test))]
pub fn enabled() -> bool {
    let flags: u64;
    unsafe { asm!("pushfq", "pop {}", out(reg) flags, options(nomem)) };

    flags & (1 << 9) != 0
}

#[cfg(test)]
pub fn enabled() -> bool {
    true
}

#[inline]
pub unsafe fn enable() {
    #[cfg(not(test))]
    unsafe { asm!("sti", options(nomem, nostack)) };
}

#[inline]
pub fn disable() {
    #[cfg(not(test))]
    unsafe { asm!("cli", options(nomem, nostack)) };
}
