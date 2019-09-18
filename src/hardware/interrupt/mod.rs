use crate::memory::paging::VirtualAddress;

mod exception;
mod idt;

type Handler = extern "x86-interrupt" fn(_: &mut InterruptStackFrame);

#[cfg(target_arch = "x86_64")]
type HandlerErr = extern "x86-interrupt" fn(_: &mut InterruptStackFrame, err: usize);

lazy_static! {
    static ref IDT: idt::IDT = {
        let mut idt = idt::IDT::new();
        idt.set_handler(0, exception::divide_by_zero);
        idt.set_handler(3, exception::breakpoint);
        idt.set_handler(6, exception::invalid_opcode);
        idt.set_handler(8, exception::double_fault);
        idt.set_handler(14, exception::page_fault);
        idt
    };
}

#[cfg(target_arch = "x86_64")]
#[repr(packed)]
#[derive(Clone, Debug)]
pub struct InterruptStackFrame {
    instruction_pointer: VirtualAddress,
    code_segment: u64,
    flags: u64,
    stack_pointer: VirtualAddress,
    stack_segment: u64,
}

pub fn init() {
    IDT.load();
}
