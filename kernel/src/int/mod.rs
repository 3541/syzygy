// Interrupts.

mod arch;
pub use arch::*;

pub fn disable() -> bool {
    let ret = interrupts_enabled();
    cli();
    ret
}

pub fn set(state: bool) {
    if state {
        sti()
    } else {
        cli()
    }
}
