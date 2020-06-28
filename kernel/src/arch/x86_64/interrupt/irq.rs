use super::{InterruptController, InterruptStackFrame, InterruptVector, INTERRUPT_CONTROLLER};

macro_rules! irq_handler {
    ($type:path => fn $handler:ident($param:ident) $inner:block) => {
        pub extern "x86-interrupt" fn $handler($param: &mut InterruptStackFrame) {
            $inner;
            INTERRUPT_CONTROLLER.lock().end_of_interrupt($type);
        }
    };
}

irq_handler!(InterruptVector::Timer => fn timer(_stack) {
});

irq_handler!(InterruptVector::Keyboard => fn keyboard(_stack) {
});
