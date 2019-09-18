use core::mem::size_of;

use super::Handler;

pub struct IDT([Entry; 16]);

impl IDT {
    pub fn new() -> Self {
        IDT([Entry::missing(); 16])
    }

    pub fn set_handler(&mut self, index: u8, handler: Handler) -> &mut EntryOptions {
        self.0[index as usize] = Entry::new(current_cs(), handler);
        &mut self.0[index as usize].options
    }

    pub fn load(&'static self) {
        let p = IDTPointer {
            limit: (size_of::<Self>() - 1) as u16,
            address: self as *const _ as u64,
        };

        unsafe {
            asm!("lidt ($0)" :: "r"(&p) : "memory");
        }
    }
}

fn current_cs() -> u16 {
    let mut ret: u16 = 0;
    unsafe {
        asm!("mov %cs, $0" : "=r"(ret));
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
        let p = handler as u64;
        Entry {
            offset_low: p as u16,
            selector: selector,
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
