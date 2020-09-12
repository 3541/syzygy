// Interrupts.

mod arch;
pub use arch::*;

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
