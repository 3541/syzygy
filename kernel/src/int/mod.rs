// Interrupts.

mod arch;
#[macro_use]
mod isr;
mod exception;

use crate::util::sync::spin::{OnceCell, Spinlock, SpinlockGuard};
use crate::util::PrivilegeLevel;
use arch::{cli, interrupts_enabled, sti};
pub use arch::{Idt, InterruptVector};

// Architecture implementations should conform to this trait.
pub trait InterruptTable: Sized {
    type Isr;
    type InterruptVector;

    fn new() -> Self;
    fn the() -> SpinlockGuard<'static, Self>;
    unsafe fn set_vector(
        &mut self,
        vector: Self::InterruptVector,
        handler: Self::Isr,
        privilege: PrivilegeLevel,
    );
    unsafe fn load(&self);
}

static IDT: OnceCell<Spinlock<Idt>> = OnceCell::new();

pub fn disable() -> bool {
    let ret = interrupts_enabled();
    cli();
    ret
}

pub fn enable() -> bool {
    let ret = interrupts_enabled();
    sti();
    ret
}

pub fn init() {
    arch::init()
}
