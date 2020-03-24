use logc::error;

use crate::memory::RawPhysicalAddress;

#[derive(Debug)]
#[repr(packed)]
pub struct StackFrame {
    rbp: *const StackFrame,
    rip: *const u64,
}

#[panic_handler]
#[cfg(not(test))]
fn panic(info: &core::panic::PanicInfo) -> ! {
    error!("{}", info);

    let rbp: *const StackFrame;
    unsafe {
        asm!("movq %rbp, %rax" : "=r"(rbp));
        print_backtrace(&*rbp);
    }

    loop {}
}

pub unsafe fn print_backtrace(mut stack_frame: &StackFrame) {
    error!("-----");
    while stack_frame.rbp as RawPhysicalAddress != 0 {
        error!("{:x?}", stack_frame);
        stack_frame = &*stack_frame.rbp;
    }
    error!("-----");
}
