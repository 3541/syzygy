/*
 * E9: Debug port logging.
 *
 * Copyright (c) 2020, 2024 Alex O'Brien <3541@3541.website>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of
 * the MPL was not distributed with this file, you can obtain one at http://mozilla.org/MPL/2.0/.
 */

use core::fmt::{self, Write};

use ansi_rgb::Foreground;
use rgb::RGB8;

use crate::io::{arch::port::Port, log::Color};

pub struct DebugPort(Port<u8>);

impl DebugPort {
    const ADDRESS: u16 = 0xE9;

    fn the() -> &'static Self {
        static INSTANCE: DebugPort = Self(Port::new(DebugPort::ADDRESS));
        &INSTANCE
    }
}

impl fmt::Write for &DebugPort {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for byte in s.bytes() {
            unsafe {
                self.0.write(byte);
            }
        }

        Ok(())
    }
}

impl From<Color> for RGB8 {
    fn from(val: Color) -> Self {
        match val {
            Color::Gray => RGB8::new(98, 98, 98),
            Color::Cyan => ansi_rgb::cyan(),
            Color::Green => ansi_rgb::green(),
            Color::LightGreen => ansi_rgb::yellow_green(),
            Color::Yellow => ansi_rgb::yellow(),
            Color::Red => ansi_rgb::red(),
        }
    }
}

#[doc(hidden)]
pub(in crate::io::log) fn print(args: fmt::Arguments) {
    write!(DebugPort::the(), "{}", args).unwrap();
}

#[doc(hidden)]
pub(in crate::io::log) fn print_colored(color: Color, args: fmt::Arguments) {
    write!(DebugPort::the(), "{}", args.fg(color.into())).unwrap();
}
