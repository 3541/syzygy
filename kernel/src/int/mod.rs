// Interrupts.

#[macro_use]
pub mod arch;
mod exception;
mod isr;

use crate::util::sync::spin::{OnceCell, Spinlock, SpinlockGuard};
use crate::util::PrivilegeLevel;
use arch::{cli, interrupts_enabled, sti};
pub use arch::{Idt, InterruptVector};

// Architecture implementations should conform to this trait.
pub trait InterruptTable: Sized {
    type Handler;
    type InterruptVector;

    fn new() -> Self;
    fn the() -> SpinlockGuard<'static, Self>;
    unsafe fn set_vector(
        &mut self,
        vector: Self::InterruptVector,
        handler: Self::Handler,
        privilege: PrivilegeLevel,
    );
    unsafe fn load(&self);
}

static IDT: OnceCell<Spinlock<Idt>> = OnceCell::new();

#[cfg(not(test))]
pub fn disable() -> bool {
    let ret = interrupts_enabled();
    cli();
    ret
}

#[cfg(test)]
pub fn disable() -> bool {
    false
}

#[cfg(not(test))]
pub fn enable() -> bool {
    let ret = interrupts_enabled();
    sti();
    ret
}

#[cfg(test)]
pub fn enable() -> bool {
    false
}

pub fn init() {
    arch::init()
}
