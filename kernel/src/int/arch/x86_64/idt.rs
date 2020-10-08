use core::mem::{size_of, transmute};

use super::{Handler, HandlerCode, InterruptVector};
use crate::int::{exception, InterruptTable, IDT};
use crate::util::arch::register;
use crate::util::sync::spin::{Spinlock, SpinlockGuard};
use crate::util::PrivilegeLevel;

#[derive(Copy, Clone)]
#[repr(C, packed)]
struct IdtEntry {
    offset_low: u16,
    selector: u16,
    ist_offset: u8,
    attr: u8,
    offset_mid: u16,
    offset_high: u32,
    _zero: u32,
}

impl IdtEntry {
    fn new(
        selector: u16,
        handler: <Idt as InterruptTable>::Handler,
        privilege_level: PrivilegeLevel,
    ) -> IdtEntry {
        let handler = handler as u64;
        IdtEntry {
            offset_low: handler as u16,
            selector,
            ist_offset: 0,
            attr: (1 << 7) | ((privilege_level as u8) << 5) | 0b1110, // Present interrupt gate.
            offset_mid: (handler >> 16) as u16,
            offset_high: (handler >> 32) as u32,
            _zero: 0,
        }
    }

    const fn null() -> IdtEntry {
        IdtEntry {
            offset_low: 0,
            selector: 0,
            ist_offset: 0,
            attr: 0b1110, // Non-present interrupt gate.
            offset_mid: 0,
            offset_high: 0,
            _zero: 0,
        }
    }
}

#[repr(C, packed)]
struct Idtr {
    limit: u16,
    base: u64,
}

pub struct Idt([IdtEntry; 256]);

impl Idt {
    fn null() -> Idt {
        Idt([IdtEntry::null(); 256])
    }

    // Convenience wrapper for setting an exception handler with an error code.
    unsafe fn set_vector_code(
        &mut self,
        vector: InterruptVector,
        handler: HandlerCode,
        privilege: PrivilegeLevel,
    ) {
        self.set_vector(vector, transmute(handler), privilege)
    }
}

impl InterruptTable for Idt {
    type Handler = Handler;
    type InterruptVector = InterruptVector;

    fn new() -> Self {
        let mut ret = Idt::null();

        unsafe {
            ret.set_vector(
                InterruptVector::DivideByZero,
                exception::divide_by_zero,
                PrivilegeLevel::User,
            );
            ret.set_vector(
                InterruptVector::Overflow,
                exception::overflow,
                PrivilegeLevel::User,
            );
            ret.set_vector(
                InterruptVector::BoundsRange,
                exception::bounds_range,
                PrivilegeLevel::User,
            );
            ret.set_vector(
                InterruptVector::InvalidOpcode,
                exception::invalid_opcode,
                PrivilegeLevel::User,
            );
            ret.set_vector_code(
                InterruptVector::DoubleFault,
                exception::double_fault,
                PrivilegeLevel::User,
            );
            ret.set_vector_code(
                InterruptVector::StackSegment,
                exception::stack_segment,
                PrivilegeLevel::User,
            );
            ret.set_vector_code(
                InterruptVector::GeneralProtectionFault,
                exception::general_protection_fault,
                PrivilegeLevel::User,
            );
            ret.set_vector_code(
                InterruptVector::PageFault,
                exception::page_fault,
                PrivilegeLevel::User,
            );
        }

        ret
    }

    fn the() -> SpinlockGuard<'static, Idt> {
        IDT.lock()
    }

    unsafe fn set_vector(
        &mut self,
        vector: InterruptVector,
        handler: Handler,
        privilege: PrivilegeLevel,
    ) {
        let vector: u8 = vector.into();
        self.0[vector as usize] = IdtEntry::new(register::read::cs(), handler, privilege);
    }

    unsafe fn load(&self) {
        let p = Idtr {
            limit: (size_of::<Idt>() - 1) as u16,
            base: self as *const _ as u64,
        };

        llvm_asm!("lidt ($0)" :: "r"(&p) : "memory");
    }
}

pub fn init() {
    IDT.init(Spinlock::new(Idt::new()));
    unsafe { IDT.lock().load() };
}
