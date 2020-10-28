pub mod cpuid;
pub mod driver;
pub mod gdt;
#[macro_use]
pub mod interrupt;
pub mod port;
pub mod register;
pub mod ring;
pub mod constants {
    use crate::memory::VirtualAddress;

    pub const KERNEL_BASE: VirtualAddress =
        unsafe { VirtualAddress::new_const(0xFFFFC00000000000) };
}

pub use ring::PrivilegeLevel;

use port::Port;

const WAIT_PORT: u16 = 0x80;

#[inline]
pub fn io_wait() {
    unsafe { Port::<u8>::new(WAIT_PORT).write(0) }
}

#[inline]
pub fn enable_interrupts_and_halt() {
    unsafe { asm!("sti", "hlt", options(nomem, nostack)) }
}

#[inline]
pub fn mfence() {
    unsafe { asm!("mfence") }
}

#[inline]
pub fn pause() {
    unsafe { asm!("pause") }
}

pub fn halt_loop() -> ! {
    loop {
        enable_interrupts_and_halt()
    }
}
