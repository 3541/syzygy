use core::fmt::{self, Write};

use crate::Port;

const DEBUG_PORT_ADDRESS: u16 = 0xE9;

pub static mut DEBUG_PORT: DebugPort = DebugPort::new(DEBUG_PORT_ADDRESS);

pub struct DebugPort(Port<u8>);

impl DebugPort {
    const fn new(port_address: u16) -> DebugPort {
        DebugPort(Port::new(port_address))
    }

    fn write_byte(&mut self, byte: u8) {
        unsafe { self.0.write(byte) }
    }
}

impl Write for DebugPort {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for byte in s.bytes() {
            self.write_byte(byte)
        }
        Ok(())
    }
}

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    unsafe {
        DEBUG_PORT
            .write_fmt(args)
            .expect("Failed to print to debug port.")
    };
}
