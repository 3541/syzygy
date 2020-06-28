use core::mem::{size_of, transmute};

use super::{exception, irq, Handler, HandlerErr, InterruptVector};

pub struct Idt([Entry; 40]);

impl Idt {
    pub fn empty() -> Idt {
        Idt([Entry::missing(); 40])
    }

    pub fn new() -> Idt {
        let mut ret = Idt::empty();

        ret.set_handler(InterruptVector::DivideByZero, exception::divide_by_zero);
        ret.set_handler(InterruptVector::Breakpoint, exception::breakpoint);
        ret.set_handler(InterruptVector::InvalidOpcode, exception::invalid_opcode);
        ret.set_handler(InterruptVector::DoubleFault, exception::double_fault);
        ret.set_handler_errc(
            InterruptVector::GeneralProtectionFault,
            exception::general_protection_fault,
        );
        ret.set_handler_errc(InterruptVector::PageFault, exception::page_fault);

        ret.set_handler(InterruptVector::Timer, irq::timer);
        ret.set_handler(InterruptVector::Keyboard, irq::keyboard);

        ret
    }

    pub fn set_handler(&mut self, vector: InterruptVector, handler: Handler) -> &mut EntryOptions {
        let vector: u8 = vector as u8;
        self.0[vector as usize] = Entry::new(current_cs(), handler);
        &mut self.0[vector as usize].options
    }

    pub fn set_handler_errc(
        &mut self,
        vector: InterruptVector,
        handler: HandlerErr,
    ) -> &mut EntryOptions {
        self.set_handler(vector, unsafe { transmute(handler) })
    }

    pub fn load(&self) {
        let p = IDTPointer {
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
struct IDTPointer {
    limit: u16,
    #[cfg(target_arch = "x86_64")]
    address: u64,
    #[cfg(target_arch = "x86")]
    address: u32,
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
    fn new(selector: u16, handler: Handler) -> Self {
        let p = handler as usize as u64;
        Entry {
            offset_low: p as u16,
            selector,
            ist_offset: 0,
            options: EntryOptions::new(true, GateType::Interrupt, 0),
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

#[repr(C, packed)]
#[cfg(target_arch = "x86")]
struct Entry {
    offset_low: u16,
    selector: u16,
    _zero: u8,
    options: u8,
    offset_high: u16,
}

#[derive(Debug, Clone, Copy)]
pub struct EntryOptions(u8);

impl EntryOptions {
    fn new(present: bool, gate_type: GateType, privilege_level: u8) -> Self {
        EntryOptions(
            if present { 1 } else { 0 } << 7
                | privilege_level << 5
                | match gate_type {
                    GateType::Interrupt => 0b1110,
                    GateType::Trap => 0b1111,
                },
        )
    }

    fn missing() -> Self {
        EntryOptions(0b00001110)
    }
}

enum GateType {
    Interrupt,
    Trap,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn entry_options() {
        assert_eq!(
            EntryOptions::new(true, GateType::Interrupt, 0b11).0,
            0b11101110
        );

        assert_eq!(EntryOptions::new(false, GateType::Trap, 0b01).0, 0b00101111);
    }
}
