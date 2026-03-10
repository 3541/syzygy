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

common::arch_mod!();

pub struct InterruptDisableGuard(bool);

impl InterruptDisableGuard {
    pub fn new() -> Self {
        let enabled = enabled();
        disable();
        Self(enabled)
    }
}

impl Drop for InterruptDisableGuard {
    fn drop(&mut self) {
        if self.0 {
            unsafe { enable() };
        }
    }
}
