use core::marker::PhantomData;

pub struct Port<S> {
    address: u16,
    size: PhantomData<S>,
}

impl<S> Port<S> {
    pub const fn new(address: u16) -> Port<S> {
        Port {
            address,
            size: PhantomData,
        }
    }
}

impl Port<u8> {
    pub unsafe fn write(&mut self, value: u8) {
        asm!("out dx, al", in("dx") self.address, in("al") value, options(nostack, nomem));
    }
}
