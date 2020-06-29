mod apic;
mod exception;
mod idt;
mod irq;
mod pic;

use logc::info;
use spin::{Mutex, MutexGuard, Once};

use crate::arch::cpuid;
use crate::memory::VirtualAddress;

use apic::Apic;
use idt::Idt;
use pic::PicChain;

type Handler = extern "x86-interrupt" fn(&mut InterruptStackFrame);

#[cfg(target_arch = "x86_64")]
type HandlerErr = extern "x86-interrupt" fn(&mut InterruptStackFrame, usize);

pub static INTERRUPT_CONTROLLER: Mutex<Controller> = Mutex::new(Controller::Uninitialized);
pub static IDT: Once<Mutex<Idt>> = Once::new();

pub enum Controller {
    Pic(PicChain),
    Apic(Apic),
    Uninitialized,
}

unsafe impl InterruptController for Controller {
    fn end_of_interrupt(&mut self, interrupt: InterruptVector) {
        match self {
            Controller::Pic(chain) => chain.end_of_interrupt(interrupt),
            Controller::Apic(apic) => apic.end_of_interrupt(interrupt),
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
    Timer = PicChain::PIC1_OFFSET,
    Keyboard,
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

pub fn without_interrupts<T>(f: impl Fn() -> T) -> T {
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

fn idt() -> MutexGuard<'static, Idt> {
    IDT.call_once(|| Mutex::new(Idt::new())).lock()
}

pub fn load_idt() {
    idt().load();
}

pub fn init() {
    let mut controller = INTERRUPT_CONTROLLER.lock();

    if cpuid::has_apic() {
        info!("CPU has an APIC.");
    } else {
        info!("CPU does not have an APIC.");
    }
    let mut pic = PicChain::new();
    unsafe { pic.init() };
    *controller = Controller::Pic(pic);
    enable();
}
