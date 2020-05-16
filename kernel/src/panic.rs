use alloc::string::{String, ToString};

use logc::error;
use rustc_demangle::demangle;

use crate::memory::{Address, RawVirtualAddress, VirtualAddress};
use crate::sym::SYMBOLS;

#[derive(Debug, Copy, Clone)]
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
        llvm_asm!("movq %rbp, %rax" : "=r"(rbp));
        print_backtrace(&*rbp);
    }

    loop {}
}

pub unsafe fn print_backtrace(mut stack_frame: &StackFrame) {
    let syms = SYMBOLS.get();
    error!("-----");
    while stack_frame.rbp as RawVirtualAddress != 0 {
        if let Some(s) = syms.find(VirtualAddress::new(stack_frame.rip as RawVirtualAddress)) {
            error!("{:#}", demangle(s));
        } else {
            error!("{:?} (No symbol found. Probably unloaded.)", stack_frame);
        }
        stack_frame = &*stack_frame.rbp;
    }
    error!("-----");
}
