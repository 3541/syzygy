//! I/O port access.

use core::marker::PhantomData;

/// An I/O port.
pub struct Port<S> {
    address: u16,
    size: PhantomData<S>,
}

impl<S> Port<S> {
    /// Create a new port with the given address, of size `S`.
    pub const fn new(address: u16) -> Port<S> {
        Port {
            address,
            size: PhantomData,
        }
    }
}

impl Port<u8> {
    /// Write a byte to the port.
    /// # Safety
    /// Ports can perform a wide variety of actions, so the safety
    /// characteristics may vary wildly. There is nothing inherently unsafe
    /// about writing to a port.
    pub unsafe fn write(&mut self, value: u8) {
        asm!("out dx, al", in("dx") self.address, in("al") value, options(nostack, nomem));
    }
}
