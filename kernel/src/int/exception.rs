use super::arch::InterruptStackFrame;
use crate::util::halt_loop;
use log_crate::error;

fn fatal(name: &str, stack: &InterruptStackFrame) -> ! {
    error!("FATAL EXCEPTION: {}\n {:#x?}", name, stack);
    halt_loop();
}

macro_rules! fatal_exception {
    ($vec:path => $name:ident) => {
        isr_fn!($vec => fn $name(stack) {
            fatal(stringify!($name), stack);
        });
    };
}

fatal_exception!(InterruptVector::DivideByZero => divide_by_zero);
fatal_exception!(InterruptVector::InvalidOpcode => invalid_opcode);
fatal_exception!(InterruptVector::DoubleFault => double_fault);
fatal_exception!(InterruptVector::GeneralProtectionFault => general_protection_fault);
