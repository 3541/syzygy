//! # Utility types and functions.

#[macro_use]
pub mod id;

pub mod arch;
pub mod either;
pub mod sync;

pub use arch::register;
pub use either::Either;

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
