//! # Interrupt handing and interrupt controllers.

#[macro_use]
pub mod arch;
mod exception;
mod isr;

pub use arch::interrupts_enabled as enabled;
pub use arch::{Idt, InterruptVector};

use crate::util::sync::spin::{Spinlock, SpinlockGuard};
use crate::util::sync::OnceCell;
use crate::util::PrivilegeLevel;
use arch::{cli, sti};

/// An interrupt table. Architecture-specific implementations should conform to this trait.
pub trait InterruptTable: Sized {
    /// A function pointer of some kind referring to an ISR.
    type Handler;
    /// An index into the table.
    type InterruptVector;

    /// Create a default table.
    fn new() -> Self;

    /// Get the current table.
    fn the() -> SpinlockGuard<'static, Self>;

    /// Set the handler at the given index.
    /// # Safety
    /// The given handler must be a safe interrupt handler, and must perform any required functions of the given vector.
    unsafe fn set_vector(
        &mut self,
        vector: Self::InterruptVector,
        handler: Self::Handler,
        privilege: PrivilegeLevel,
    );

    /// Activate the table.
    /// # Safety
    /// All previously set handlers must be safe, and the table must not be missing any required handlers.
    unsafe fn load(&self);
}

/// The current [IDT](Idt).
static IDT: OnceCell<Spinlock<Idt>> = OnceCell::new();

/// Disable interrupts and return whether they were previously enabled.
#[cfg(not(test))]
pub fn disable() -> bool {
    let ret = enabled();
    cli();
    ret
}

#[cfg(test)]
pub fn disable() -> bool {
    false
}

/// Enable interrupts and return whether they were previously enabled.
#[cfg(not(test))]
pub fn enable() -> bool {
    let ret = enabled();
    sti();
    ret
}

/// Enable interrupts and halt immediately.
#[cfg(not(test))]
pub fn enable_and_halt() {
    unsafe { asm!("sti; hlt") }
}

#[cfg(test)]
pub fn enable() -> bool {
    false
}

#[cfg(test)]
pub fn enable_and_halt() {}

/// Initialize interrupt controllers and timers.
pub fn init() {
    arch::init();
    enable();
}
