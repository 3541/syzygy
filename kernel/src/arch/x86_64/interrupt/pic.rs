use super::{InterruptController, InterruptVector};
use crate::arch::io_wait;
use crate::arch::port::Port;
use crate::debug;

#[repr(u8)]
enum Command {
    Init = 0x11,
    EndOfInterrupt = 0x20,
    Mode8086 = 1,
}

struct Pic {
    offset: u8,
    command: Port<u8>,
    data: Port<u8>,
}

impl Pic {
    #[inline]
    fn end_of_interrupt(&mut self) {
        unsafe { self.command.write(Command::EndOfInterrupt as u8) }
    }

    #[inline]
    fn services_interrupt(&self, interrupt: InterruptVector) -> bool {
        let interrupt = interrupt as u8;
        interrupt >= self.offset && interrupt < self.offset + 8
    }
}

pub struct PicChain(Pic, Pic);

// TODO: Move to the correct offset if actually used.
impl PicChain {
    const PIC1_COMMAND_ADDRESS: u16 = 0x20;
    const PIC1_DATA_ADDRESS: u16 = Self::PIC1_COMMAND_ADDRESS + 1;
    pub const PIC1_OFFSET: u8 = 32;

    const PIC2_COMMAND_ADDRESS: u16 = 0xA0;
    const PIC2_DATA_ADDRESS: u16 = Self::PIC2_COMMAND_ADDRESS + 1;
    pub const PIC2_OFFSET: u8 = Self::PIC1_OFFSET + 8;

    pub unsafe fn new() -> PicChain {
        let mut ret = PicChain(
            Pic {
                offset: PicChain::PIC1_OFFSET,
                command: Port::new(PicChain::PIC1_COMMAND_ADDRESS),
                data: Port::new(PicChain::PIC1_DATA_ADDRESS),
            },
            Pic {
                offset: PicChain::PIC2_OFFSET,
                command: Port::new(PicChain::PIC2_COMMAND_ADDRESS),
                data: Port::new(PicChain::PIC2_DATA_ADDRESS),
            },
        );
        ret.init();
        ret
    }

    pub fn mask_all(&mut self) {
        unsafe {
            self.0.data.write(0xFF);
            self.1.data.write(0xFF);
        }
    }

    /// # Safety
    /// Caller must guarantee that:
    /// * PIC is in a correct state to be initialized.
    pub unsafe fn init(&mut self) {
        self.0.command.write(Command::Init as u8);
        io_wait();
        self.1.command.write(Command::Init as u8);
        io_wait();

        debug!("Initializing PIC1 with offset {}", self.0.offset);
        self.0.data.write(self.0.offset);
        io_wait();
        debug!("Initializing PIC2 with offset {}", self.1.offset);
        self.1.data.write(self.1.offset);
        io_wait();

        self.0.data.write(1 << 2); // Slave at IRQ2
        io_wait();
        self.1.data.write(2); // Cascade
        io_wait();

        self.0.data.write(Command::Mode8086 as u8);
        io_wait();
        self.1.data.write(Command::Mode8086 as u8);
        io_wait();

        // All lines masked except IRQ2 the first PIC.
        self.0.data.write(0xFF & !(1 << 2));
        self.1.data.write(0xFF);
    }
}

unsafe impl InterruptController for PicChain {
    fn end_of_interrupt(&mut self, interrupt: InterruptVector) {
        if self.1.services_interrupt(interrupt) {
            self.1.end_of_interrupt();
        }

        if self.0.services_interrupt(interrupt) || self.1.services_interrupt(interrupt) {
            self.0.end_of_interrupt()
        }
    }

    fn disable(&mut self) {
        self.mask_all();
    }
}
