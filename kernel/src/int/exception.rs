use super::arch::InterruptStackFrame;
use crate::util::halt_loop;
use log_crate::error;

fn fatal(name: &str, stack: &InterruptStackFrame, code: Option<usize>) -> ! {
    if let Some(code) = code {
        error!("FATAL EXCEPTION: {} - {}\n {:#x?}", name, code, stack);
    } else {
        error!("FATAL EXCEPTION: {}\n {:#x?}", name, stack);
    }
    halt_loop();
}

macro_rules! fatal_exception {
    ($vec:path => fn $name:ident) => {
        handler_fn!($vec => fn $name(stack) {
            fatal(stringify!($name), stack, None);
        });
    };

    ($vec:path => fn $name:ident(code)) => {
        handler_fn!($vec => fn $name(stack, code: usize) {
            fatal(stringify!($name), stack, Some(code));
        });
    };
}

fatal_exception!(InterruptVector::DivideByZero => fn divide_by_zero);
fatal_exception!(InterruptVector::InvalidOpcode => fn invalid_opcode);
fatal_exception!(InterruptVector::DoubleFault => fn double_fault(code));
fatal_exception!(InterruptVector::GeneralProtectionFault => fn general_protection_fault);
