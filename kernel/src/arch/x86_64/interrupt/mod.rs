// Elevated so irq_handler is available in other submodules.
#[macro_use]
mod irq;

mod apic;
mod exception;
mod idt;
mod pic;

pub use apic::LvtFlags;
pub use idt::Idt;

use logc::debug;

use crate::arch::cpuid;
use crate::memory::VirtualAddress;
use crate::sync::SpinLock;

use apic::LocalApic;
use pic::PicChain;

type Handler = extern "x86-interrupt" fn(&mut InterruptStackFrame);

#[cfg(target_arch = "x86_64")]
type HandlerErr = extern "x86-interrupt" fn(&mut InterruptStackFrame, usize);

pub static INTERRUPT_CONTROLLER: SpinLock<Controller> = SpinLock::new(Controller::Uninitialized);

pub enum Controller {
    Pic(PicChain),
    LocalApic(LocalApic),
    Uninitialized,
}

unsafe impl InterruptController for Controller {
    fn end_of_interrupt(&mut self, interrupt: InterruptVector) {
        match self {
            Controller::Pic(chain) => chain.end_of_interrupt(interrupt),
            Controller::LocalApic(apic) => apic.end_of_interrupt(interrupt),
            Controller::Uninitialized => {
                panic!("Tried to EOI an uninitialized InterruptController.")
            }
        }
    }

    fn disable(&mut self) {
        panic!("Tried to disable the generic interrupt controller.");
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum InterruptVector {
    DivideByZero = 0,
    Breakpoint = 3,
    InvalidOpcode = 6,
    DoubleFault = 8,
    GeneralProtectionFault = 13,
    PageFault = 14,
    Timer = 0x30,

    Spurious = 255,
}

pub unsafe trait InterruptController {
    fn end_of_interrupt(&mut self, interrupt: InterruptVector);
    fn disable(&mut self);
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

pub fn is_enabled() -> bool {
    let flags: u64;
    unsafe {
        asm!(
            "pushfq",
            "pop {}",
            out(reg) flags
        )
    };

    flags & (1 << 9) != 0
}

#[inline(always)]
pub fn enable() {
    unsafe { llvm_asm!("sti" :::: "volatile") }
}

#[inline(always)]
pub fn disable() {
    unsafe { llvm_asm!("cli" :::: "volatile") }
}

pub fn without_interrupts<T>(f: impl FnOnce() -> T) -> T {
    let was_enabled = is_enabled();
    if was_enabled {
        disable();
    }
    let ret = f();
    if was_enabled {
        enable();
    }
    ret
}

pub fn init() {
    let mut pic = unsafe { PicChain::new() };

    if cpuid::has_apic() {
        debug!("CPU has an APIC.");

        pic.disable();
        debug!("PIC disabled.");

        *INTERRUPT_CONTROLLER.lock() = Controller::LocalApic(LocalApic::new());
    } else {
        debug!("CPU does not have an APIC.");
        *INTERRUPT_CONTROLLER.lock() = Controller::Pic(pic);
    }
}
