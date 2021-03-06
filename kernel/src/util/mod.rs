//! # Utility types and functions.

pub mod arch;
pub mod either;
pub mod sync;

pub use arch::register;

/// Halt forever.
pub fn halt_loop() -> ! {
    loop {
        unsafe { asm!("hlt") };
    }
}

/// Kernel mode or user mode.
#[repr(u8)]
#[derive(Copy, Clone)]
pub enum PrivilegeLevel {
    Kernel = 0,
    User = 3,
}
