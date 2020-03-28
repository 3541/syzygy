use core::sync::atomic::{AtomicUsize, Ordering};

use lazy_static::lazy_static;

use crate::memory::VirtualAddress;
use crate::print;

mod exception;
mod idt;
mod pic;

use pic::{PIC1_OFFSET, PICS};

type Handler = extern "x86-interrupt" fn(&mut InterruptStackFrame);

#[cfg(target_arch = "x86_64")]
type HandlerErr = extern "x86-interrupt" fn(&mut InterruptStackFrame, usize);

macro_rules! pic_handler {
    ($type:path => fn $handler:ident($param:ident) $inner:block) => {
        extern "x86-interrupt" fn $handler($param: &mut InterruptStackFrame) {
            $inner;
            unsafe { PICS.lock().end_of_interrupt($type) };
        }
    };
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum InterruptIndex {
    DivideByZero = 0,
    Breakpoint = 3,
    InvalidOpcode = 6,
    DoubleFault = 8,
    PageFault = 14,
    Timer = PIC1_OFFSET,
}

impl Into<u8> for InterruptIndex {
    fn into(self) -> u8 {
        self as u8
    }
}

#[cfg(target_arch = "x86_64")]
#[repr(C, packed)]
#[derive(Debug, Copy, Clone)]
pub struct InterruptStackFrame {
    instruction_pointer: VirtualAddress,
    code_segment: u64,
    flags: u64,
    stack_pointer: VirtualAddress,
    stack_segment: u64,
}

lazy_static! {
    static ref IDT: idt::IDT = {
        let mut idt = idt::IDT::new();
        idt.set_handler(InterruptIndex::DivideByZero, exception::divide_by_zero);
        idt.set_handler(InterruptIndex::Breakpoint, exception::breakpoint);
        idt.set_handler(InterruptIndex::InvalidOpcode, exception::invalid_opcode);
        idt.set_handler(InterruptIndex::DoubleFault, exception::double_fault);
        idt.set_handler_errc(InterruptIndex::PageFault, exception::page_fault);

        idt.set_handler(InterruptIndex::Timer, timer);

        idt
    };
}

pic_handler!(InterruptIndex::Timer => fn timer(_stack) {
//    print!(".")
});

static DISABLE_COUNT: AtomicUsize = AtomicUsize::new(1);

#[inline]
pub fn enable() {
    if DISABLE_COUNT.fetch_sub(1, Ordering::SeqCst) == 1 {
        unsafe { asm!("sti" :::: "volatile") }
    }
}

#[inline]
pub fn disable() {
    if DISABLE_COUNT.fetch_add(1, Ordering::SeqCst) == 0 {
        unsafe { asm!("cli" :::: "volatile") }
    }
}

pub fn init() {
    IDT.load();
    unsafe { PICS.lock().init() };
    enable();
}

pub fn without_interrupts<T>(f: impl Fn() -> T) -> T {
    disable();
    let ret = f();
    enable();
    ret
}
