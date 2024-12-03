/*
 * ARCH: Platform submodule helper.
 *
 * These macros define an appropriately-named submodule for each supported CPU
 * architecture and reexport all symbols from the correct target.
 *
 * Copyright (c) 2020, 2024 Alex O'Brien <3541@3541.website>
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

#[macro_export]
macro_rules! arch_mod_impl {
    ($arch:ident, $arch_str:expr) => {
        #[cfg(target_arch = $arch_str)]
        mod $arch;
        #[cfg(target_arch = $arch_str)]
        pub use $arch::*;
    };
}

#[macro_export]
macro_rules! arch_mod {
    () => {
        common::arch_mod_impl!(amd64, "x86_64");
        common::arch_mod_impl!(aarch64, "aarch64");
    };
}
