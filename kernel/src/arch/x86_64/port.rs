use core::marker::PhantomData;

pub struct Port<T: PortAccess> {
    address: u16,
    size: PhantomData<T>,
}

impl<T: PortAccess> Port<T> {
    pub const fn new(address: u16) -> Port<T> {
        Port {
            address,
            size: PhantomData,
        }
    }
}

impl<T: PortAccess> Port<T> {
    #[inline]
    pub unsafe fn read(&self) -> T {
        T::port_read(self.address)
    }

    #[inline]
    pub unsafe fn write(&mut self, value: T) {
        T::port_write(self.address, value)
    }
}

pub trait PortAccess {
    unsafe fn port_read(address: u16) -> Self;
    unsafe fn port_write(address: u16, value: Self);
}

impl PortAccess for u8 {
    #[inline]
    unsafe fn port_read(address: u16) -> u8 {
        let ret: u8;
        llvm_asm!("inb %dx, %al" : "={al}"(ret) : "{dx}"(address) :: "volatile");
        ret
    }

    #[inline]
    unsafe fn port_write(address: u16, value: u8) {
        llvm_asm!("outb %al, %dx" :: "{dx}"(address), "{al}"(value) :: "volatile");
    }
}

impl PortAccess for u16 {
    #[inline]
    unsafe fn port_read(address: u16) -> u16 {
        let ret: u16;
        llvm_asm!("inw %dx, %ax" : "={ax}"(ret) : "{dx}"(address) :: "volatile");
        ret
    }

    #[inline]
    unsafe fn port_write(address: u16, value: u16) {
        llvm_asm!("outw %ax, %dx" :: "{dx}"(address), "{ax}"(value) :: "volatile");
    }
}

impl PortAccess for u32 {
    #[inline]
    unsafe fn port_read(address: u16) -> u32 {
        let ret: u32;
        llvm_asm!("inl %dx, %eax" : "={eax}"(ret) : "{dx}"(address) :: "volatile");
        ret
    }

    #[inline]
    unsafe fn port_write(address: u16, value: u32) {
        llvm_asm!("outl %eax, %dx" :: "{dx}"(address), "{eax}"(value) :: "volatile");
    }
}
