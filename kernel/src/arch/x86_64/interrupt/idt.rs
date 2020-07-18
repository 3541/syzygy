use core::mem::{size_of, transmute};

use spin::{Mutex, MutexGuard, Once};

use super::{exception, irq, Handler, HandlerErr, InterruptVector};
use crate::arch::PrivilegeLevel;

pub struct Idt([Entry; 256]);

impl Idt {
    pub fn the() -> MutexGuard<'static, Idt> {
        static IDT: Once<Mutex<Idt>> = Once::new();
        IDT.call_once(|| Mutex::new(Idt::new())).lock()
    }

    pub fn empty() -> Idt {
        Idt([Entry::missing(); 256])
    }

    pub fn new() -> Idt {
        let mut ret = Idt::empty();

        // Exceptions
        ret.set_handler(
            InterruptVector::DivideByZero,
            exception::divide_by_zero,
            PrivilegeLevel::User,
        );
        ret.set_handler(
            InterruptVector::Breakpoint,
            exception::breakpoint,
            PrivilegeLevel::User,
        );
        ret.set_handler(
            InterruptVector::InvalidOpcode,
            exception::invalid_opcode,
            PrivilegeLevel::User,
        );
        ret.set_handler(
            InterruptVector::DoubleFault,
            exception::double_fault,
            PrivilegeLevel::User,
        );
        ret.set_handler_errc(
            InterruptVector::GeneralProtectionFault,
            exception::general_protection_fault,
            PrivilegeLevel::User,
        );
        ret.set_handler_errc(
            InterruptVector::PageFault,
            exception::page_fault,
            PrivilegeLevel::User,
        );

        // Hardware
        ret.set_handler(InterruptVector::Timer, irq::timer, PrivilegeLevel::User);
        /*     ret.set_handler(
            InterruptVector::Keyboard,
            irq::keyboard,
            PrivilegeLevel::User,
        );*/
        ret.set_handler(
            InterruptVector::Spurious,
            irq::spurious,
            PrivilegeLevel::User,
        );

        ret
    }

    pub fn set_handler(
        &mut self,
        vector: InterruptVector,
        handler: Handler,
        privilege_level: PrivilegeLevel,
    ) -> &mut EntryOptions {
        let vector: u8 = vector as u8;
        self.0[vector as usize] = Entry::new(current_cs(), handler, privilege_level);
        &mut self.0[vector as usize].options
    }

    pub fn set_handler_errc(
        &mut self,
        vector: InterruptVector,
        handler: HandlerErr,
        privilege_level: PrivilegeLevel,
    ) -> &mut EntryOptions {
        self.set_handler(vector, unsafe { transmute(handler) }, privilege_level)
    }

    pub fn load(&self) {
        let p = IdtPointer {
            limit: (size_of::<Self>() - 1) as u16,
            address: self as *const _ as u64,
        };

        unsafe {
            llvm_asm!("lidt ($0)" :: "r"(&p) : "memory");
        }
    }
}

fn current_cs() -> u16 {
    let mut ret: u16;
    unsafe {
        llvm_asm!("mov %cs, $0" : "=r"(ret));
    }
    ret
}

#[repr(C, packed)]
struct IdtPointer {
    limit: u16,
    #[cfg(target_arch = "x86_64")]
    address: u64,
    #[cfg(target_arch = "x86")]
    address: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct EntryOptions(u8);

impl EntryOptions {
    fn new(present: bool, privilege_level: PrivilegeLevel) -> Self {
        EntryOptions(if present { 1 } else { 0 } << 7 | (privilege_level as u8) << 5 | 0b1110)
    }

    fn missing() -> Self {
        EntryOptions(0b00001110)
    }
}

#[cfg(target_arch = "x86_64")]
#[repr(C, packed)]
#[derive(Debug, Clone, Copy)]
struct Entry {
    offset_low: u16,
    selector: u16,
    ist_offset: u8,
    options: EntryOptions,
    offset_mid: u16,
    offset_high: u32,
    _zero: u32,
}

#[cfg(target_arch = "x86_64")]
impl Entry {
    fn new(selector: u16, handler: Handler, privilege_level: PrivilegeLevel) -> Self {
        let p = handler as usize as u64;
        Entry {
            offset_low: p as u16,
            selector,
            ist_offset: 0,
            options: EntryOptions::new(true, privilege_level),
            offset_mid: (p >> 16) as u16,
            offset_high: (p >> 32) as u32,
            _zero: 0,
        }
    }

    fn missing() -> Self {
        Entry {
            offset_low: 0,
            selector: 0,
            ist_offset: 0,
            options: EntryOptions::missing(),
            offset_mid: 0,
            offset_high: 0,
            _zero: 0,
        }
    }
}
