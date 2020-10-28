use core::mem::size_of;

use bitflags::bitflags;
use logc::{debug, trace};

use super::{InterruptController, InterruptVector};
use crate::arch::register;
use crate::memory::paging::EntryFlags;
use crate::time::arch::ApicTimer;
use crate::{Address, PhysicalAddress, PhysicalMemory, VirtualRegion};

// NOTE (Intel SDM)
// "All 32-bit registers should be accessed using 128-bit aligned 32-bit loads or stores."
// "Wider registers (64-bit or 256-bit) must be accessed using multiple 32-bit loads or stores, with all accesses being 128-bit aligned."

// TODO: make not pub
#[repr(u16)]
pub enum LocalApicRegisterOffset {
    Id = 0x20,
    Version = 0x30,
    TaskPriority = 0x80,
    EndOfInterrupt = 0xB0,
    SpuriousInterruptVector = 0xF0,
    LvtTimer = 0x320,
}

bitflags! {
    pub struct LvtFlags: u32 {
        const DELIVERY_MODE_FIXED = 0;
//        const DELIVERY_MODE_EXTINT = 0b111 << 8;
        const TIMER_MODE_ONESHOT = 0;
        const TIMER_MODE_PERIODIC = 1 << 17;
        const TIMER_MODE_TSC_DEADLINE = 1 << 18;
    }
}

pub struct LocalApic {
    registers: VirtualRegion,
    timer: ApicTimer,
}

impl LocalApic {
    pub fn new() -> LocalApic {
        let base_address =
            PhysicalAddress::new((register::read::apic_base() & ((1 << 25) - 1) << 12) as usize);
        debug!("Local APIC at {}", base_address);

        let registers = unsafe { PhysicalMemory::region(base_address, 1) }
            .map_for_kernel(EntryFlags::GLOBAL | EntryFlags::NO_EXECUTE | EntryFlags::WRITABLE)
            .expect("Unable to allocate region for APIC registers.");

        trace!("Registers: {:x?}", registers);

        let mut ret = LocalApic {
            timer: ApicTimer::new(registers.start() + LocalApicRegisterOffset::LvtTimer as usize),
            registers,
        };

        debug!(
            "id: 0b{:b}",
            ret.read_register(LocalApicRegisterOffset::Id) >> 24
        );
        let version_reg = ret.read_register(LocalApicRegisterOffset::Version);
        debug!(
            "Version: 0x{:x}, Max LVT entry: {}, suppress EOI {}",
            version_reg & 0xFF,
            (version_reg >> 16) & 0xFF,
            version_reg & (1 << 24) != 0
        );

        ret.enable();
        ret.set_task_priority(0);

        // Everything is masked by default.

        unsafe { ret.timer.arm(4000) };

        ret
    }

    /// # Safety
    /// Caller must ensure that the register write being performed is safe.
    unsafe fn write_register(&mut self, offset: LocalApicRegisterOffset, value: u32) {
        self.registers
            .start()
            .as_mut_ptr::<u32>()
            .add(offset as usize / size_of::<u32>())
            .write_volatile(value);
    }

    pub fn read_register(&self, offset: LocalApicRegisterOffset) -> u32 {
        unsafe {
            self.registers
                .start()
                .as_ptr::<u32>()
                .add(offset as usize / size_of::<u32>())
                .read_volatile()
        }
    }

    fn set_task_priority(&mut self, priority: u32) {
        unsafe { self.write_register(LocalApicRegisterOffset::TaskPriority, priority) };
    }

    /*    unsafe fn set_lvt(
        &mut self,
        item: LocalApicRegisterOffset,
        flags: LvtFlags,
        vector: InterruptVector,
    ) {
        self.write_register(item, flags.bits() | vector as u32);
    }*/

    fn enable(&mut self) {
        unsafe {
            self.write_register(
                LocalApicRegisterOffset::SpuriousInterruptVector,
                InterruptVector::Spurious as u32 | (1 << 8),
            )
        };
    }
}

unsafe impl InterruptController for LocalApic {
    fn end_of_interrupt(&mut self, _interrupt: InterruptVector) {
        unsafe { self.write_register(LocalApicRegisterOffset::EndOfInterrupt, 0) };
    }

    fn disable(&mut self) {
        todo!()
    }
}
