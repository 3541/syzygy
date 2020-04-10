const WAIT_PORT: u16 = 0x80;

pub mod driver;
pub mod interrupt;
pub mod port;
pub mod process;
pub mod constants {
    use crate::memory::VirtualAddress;

    pub const KERNEL_BASE: VirtualAddress =
        unsafe { VirtualAddress::new_const(0xFFFFC00000000000) };
}

pub use driver::serial;

use port::Port;

#[inline]
pub fn io_wait() {
    unsafe { Port::<u8>::new(WAIT_PORT).write(0) }
}

#[inline]
pub fn halt() {
    unsafe { asm!("hlt" :::: "volatile") }
}

pub fn halt_loop() -> ! {
    loop {
        halt()
    }
}
