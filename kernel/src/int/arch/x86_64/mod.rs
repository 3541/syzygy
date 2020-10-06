// x86_64 interrupts.

#[macro_use]
mod handler;
mod idt;

pub use idt::Idt;

use crate::mem::VirtualAddress;

#[derive(Copy, Clone)]
pub enum InterruptVector {
    DivideByZero,
    InvalidOpcode,
    DoubleFault,
    GeneralProtectionFault,
    Other(u8),
}

impl Into<u8> for InterruptVector {
    fn into(self) -> u8 {
        match self {
            InterruptVector::DivideByZero => 0,
            InterruptVector::InvalidOpcode => 6,
            InterruptVector::DoubleFault => 8,
            InterruptVector::GeneralProtectionFault => 13,
            InterruptVector::Other(v) => v,
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(C, packed)]
pub struct InterruptStackFrame {
    ip: VirtualAddress,
    cs: u64,
    flags: u64,
    sp: VirtualAddress,
    ss: u64,
}

type Handler = extern "x86-interrupt" fn(&mut InterruptStackFrame);
type HandlerCode = extern "x86-interrupt" fn(&mut InterruptStackFrame, usize);

#[inline(always)]
pub fn cli() {
    unsafe { llvm_asm!("cli" :::: "volatile") }
}

#[inline(always)]
pub fn sti() {
    unsafe { llvm_asm!("sti" :::: "volatile") }
}

pub fn interrupts_enabled() -> bool {
    let flags: u64;
    unsafe { asm!("pushfq", "pop {}", out(reg) flags) };

    flags & (1 << 9) != 0
}

pub fn init() {}

pub fn init_idt() {
    idt::init();
}
