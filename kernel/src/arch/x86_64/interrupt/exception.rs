use logc::error;

use super::InterruptStackFrame;

macro_rules! generic_handler_ret {
    ( $n:ident ) => {
        pub extern "x86-interrupt" fn $n(stack: &mut InterruptStackFrame) {
            error!("EXCEPTION: {} - \n {:#x?}", stringify!($n), stack);
        }
    };
}

macro_rules! generic_handler_halt {
    ( $n:ident ) => {
        pub extern "x86-interrupt" fn $n(stack: &mut InterruptStackFrame) {
            error!("EXCEPTION: {} - \n {:#x?}", stringify!($n), stack);
            loop {}
        }
    };
}

#[allow(unused_macros)]
macro_rules! errc_handler_ret {
    ( $n:ident ) => {
        pub extern "x86-interrupt" fn $n(stack: &mut InterruptStackFrame, err: usize) {
            error!(
                "EXCEPTION: {}, {:x} - \n {:#x?}",
                stringify!($n),
                err,
                stack
            );
        }
    };
}

#[allow(unused_macros)]
macro_rules! errc_handler_halt {
    ( $n:ident ) => {
        pub extern "x86-interrupt" fn $n(stack: &mut InterruptStackFrame, err: usize) {
            error!(
                "EXCEPTION: {}, {:x} - \n {:#x?}",
                stringify!($n),
                err,
                stack
            );
            loop {}
        }
    };
}

mod error_code {
    use bitflags::bitflags;
    bitflags! {
        pub struct PageFault: usize {
            const PROTECTION_VIOLATION = 1;
            const WRITE_ACCESS = 1 << 1;
            const USER_MODE = 1 << 2;
            const RESERVED_WRITE = 1 << 3;
            const INSRUCTION_FETCH = 1 << 4;
        }
    }
}

generic_handler_halt!(divide_by_zero);
generic_handler_halt!(double_fault);
generic_handler_halt!(invalid_opcode);
errc_handler_halt!(general_protection_fault);

generic_handler_ret!(breakpoint);

pub extern "x86-interrupt" fn page_fault(stack: &mut InterruptStackFrame, err: usize) {
    let mut cr2: usize;
    unsafe { llvm_asm!("mov %cr2, $0" : "=r"(cr2)) };
    crate::println!(
        "EXCEPTION: page_fault accessing 0x{:x}\n\
         {:?}\n\
         {:#x?}",
        cr2,
        error_code::PageFault::from_bits(err).unwrap(),
        stack
    );
    loop {}
}
