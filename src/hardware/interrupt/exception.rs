use super::InterruptStackFrame;

macro_rules! generic_handler_ret {
    ( $n:ident ) => {
        pub extern "x86-interrupt" fn $n(stack: &mut InterruptStackFrame) {
            crate::println!("EXCEPTION: {} - \n {:#x?}", stringify!($n), stack);
        }
    };
}

macro_rules! generic_handler_halt {
    ( $n:ident ) => {
        pub extern "x86-interrupt" fn $n(stack: &mut InterruptStackFrame) {
            crate::println!("EXCEPTION: {} - \n {:#x?}", stringify!($n), stack);
            loop {}
        }
    };
}

macro_rules! errc_handler_ret {
    ( $n:ident ) => {
        pub extern "x86-interrupt" fn $n(stack: &mut InterruptStackFrame, err: usize) {
            crate::println!(
                "EXCEPTION: {}, {:x} - \n {:#x?}",
                stringify!($n),
                err,
                stack
            );
        }
    };
}

macro_rules! errc_handler_halt {
    ( $n:ident ) => {
        pub extern "x86-interrupt" fn $n(stack: &mut InterruptStackFrame, err: usize) {
            crate::println!(
                "EXCEPTION: {}, {:x} - \n {:#x?}",
                stringify!($n),
                err,
                stack
            );
            loop {}
        }
    };
}

generic_handler_halt!(divide_by_zero);
generic_handler_halt!(double_fault);
generic_handler_halt!(invalid_opcode);

generic_handler_ret!(breakpoint);

errc_handler_halt!(page_fault);
