//! Logging to the `e9` debug port.

use core::fmt::{self, Write};

use ansi_rgb::Foreground;
use rgb::RGB8;

use crate::io::log::Color;
use crate::io::port::Port;

/// The `e9` debug port.
pub struct DebugPort(Port<u8>);

impl DebugPort {
    /// The address: `0xE9`.
    const ADDRESS: u16 = 0xE9;

    const fn new() -> DebugPort {
        DebugPort(Port::new(DebugPort::ADDRESS))
    }

    fn the() -> &'static mut DebugPort {
        static mut DEBUG_PORT: DebugPort = DebugPort::new();
        unsafe { &mut DEBUG_PORT }
    }
}

impl fmt::Write for DebugPort {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for byte in s.bytes() {
            unsafe { self.0.write(byte) };
        }
        Ok(())
    }
}

impl Into<RGB8> for Color {
    fn into(self) -> RGB8 {
        match self {
            Self::Gray => RGB8::new(98, 98, 98),
            Self::Cyan => ansi_rgb::cyan(),
            Self::Green => ansi_rgb::green(),
            Self::LightGreen => ansi_rgb::yellow_green(),
            Self::Yellow => ansi_rgb::yellow(),
            Self::Red => ansi_rgb::red(),
        }
    }
}

#[doc(hidden)]
#[allow(dead_code)]
pub fn _print(args: fmt::Arguments) {
    DebugPort::the()
        .write_fmt(args)
        .expect("Failed to print to port e9.");
}

#[doc(hidden)]
#[allow(dead_code)]
pub fn _print_colored(c: Color, args: fmt::Arguments) {
    write!(DebugPort::the(), "{}", args.fg(c.into())).unwrap();
}
