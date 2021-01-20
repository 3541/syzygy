//! Logging to the `e9` debug port.

use core::fmt::{self, Write};

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

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    DebugPort::the()
        .write_fmt(args)
        .expect("Failed to print to port e9.");
}
