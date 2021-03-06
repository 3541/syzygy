//! Exception handling.

use super::arch::InterruptStackFrame;

/// A fatal exception. Print diagnostics and halt.
fn fatal(name: &str, stack: &InterruptStackFrame, code: Option<usize>) -> ! {
    if let Some(code) = code {
        panic!("FATAL EXCEPTION: {} - {}\n {:#x?}", name, code, stack);
    } else {
        panic!("FATAL EXCEPTION: {}\n {:#x?}", name, stack);
    }
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
fatal_exception!(InterruptVector::Overflow => fn overflow);
fatal_exception!(InterruptVector::BoundsRange => fn bounds_range);
fatal_exception!(InterruptVector::InvalidOpcode => fn invalid_opcode);
fatal_exception!(InterruptVector::DoubleFault => fn double_fault(code));
fatal_exception!(InterruptVector::StackSegment => fn stack_segment(code));
fatal_exception!(InterruptVector::GeneralProtectionFault => fn general_protection_fault(code));
fatal_exception!(InterruptVector::PageFault => fn page_fault(code));
