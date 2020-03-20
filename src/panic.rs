use crate::memory::RawPhysicalAddress;

#[panic_handler]
#[cfg(not(test))]
fn panic(info: &core::panic::PanicInfo) -> ! {
    #[derive(Debug)]
    #[repr(packed)]
    struct StackFrame {
        rbp: *const StackFrame,
        rip: *const u64,
    };

    error!("{}", info);
    error!("-----");

    let rbp: *const StackFrame;

    unsafe { asm!("movq %rbp, %rax" : "=r"(rbp)) };

    let mut stack_frame: &StackFrame = unsafe { &*rbp };
    while stack_frame.rbp as RawPhysicalAddress != 0 {
        error!("{:x?}", stack_frame);
        stack_frame = unsafe { &*stack_frame.rbp };
    }

    error!("-----");
    loop {}
}
