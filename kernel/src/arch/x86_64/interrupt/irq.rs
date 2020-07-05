use logc::warn;

use super::{InterruptStackFrame, InterruptVector};

macro_rules! irq_handler {
    ($vec:path => fn $handler:ident($param:ident) $inner:block) => {
        pub extern "x86-interrupt" fn $handler($param: &mut crate::interrupt::InterruptStackFrame) {
            use crate::interrupt::{InterruptController, INTERRUPT_CONTROLLER};
            $inner;
            INTERRUPT_CONTROLLER.lock().end_of_interrupt($vec);
        }
    };
}

irq_handler!(InterruptVector::Timer => fn timer(_stack) {
    warn!("This is the generic timer ISR. It does not correctly handle anything.");
    warn!("This probably means that the timer in use has failed to modify the IDT.");
});

pub extern "x86-interrupt" fn spurious(_stack: &mut InterruptStackFrame) {}

/*irq_handler!(InterruptVector::Keyboard => fn keyboard(_stack) {
});*/
