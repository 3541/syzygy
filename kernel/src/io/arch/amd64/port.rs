//! AMD64 IO port access.

use core::{arch::asm, marker::PhantomData};

/// An IO port of a given integral size.
pub struct Port<S> {
    addr: u16,
    size: PhantomData<S>,
}

impl<S> Port<S> {
    pub const fn new(addr: u16) -> Self {
        Self {
            addr,
            size: PhantomData,
        }
    }
}

impl Port<u8> {
    /// Write a byte to the port.
    /// # Safety
    /// Depends entirely on the particular port.
    #[inline]
    pub unsafe fn write(&self, v: u8) {
        asm!("out dx, al", in("dx") self.addr, in("al") v, options(nostack, nomem));
    }
}
