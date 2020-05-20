use spin::Mutex;

use super::InterruptIndex;
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
    unsafe fn end_of_interrupt(&mut self) {
        self.command.write(Command::EndOfInterrupt as u8)
    }

    #[inline]
    fn services_interrupt(&self, interrupt: InterruptIndex) -> bool {
        let interrupt = interrupt as u8;
        interrupt >= self.offset && interrupt < self.offset + 8
    }
}

pub struct PicChain(Pic, Pic);

impl PicChain {
    const PIC1_COMMAND_ADDRESS: u16 = 0x20;
    const PIC1_DATA_ADDRESS: u16 = Self::PIC1_COMMAND_ADDRESS + 1;
    pub const PIC1_OFFSET: u8 = 32;

    const PIC2_COMMAND_ADDRESS: u16 = 0xA0;
    const PIC2_DATA_ADDRESS: u16 = Self::PIC2_COMMAND_ADDRESS + 1;
    pub const PIC2_OFFSET: u8 = Self::PIC1_OFFSET + 8;

    pub unsafe fn init(&mut self) {
        let mask0 = self.0.data.read();
        let mask1 = self.1.data.read();

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

        self.0.data.write(mask0);
        self.1.data.write(mask1);
    }

    pub unsafe fn end_of_interrupt(&mut self, interrupt: InterruptIndex) {
        if self.1.services_interrupt(interrupt) {
            self.1.end_of_interrupt();
        }

        if self.0.services_interrupt(interrupt) || self.1.services_interrupt(interrupt) {
            self.0.end_of_interrupt()
        }
    }
}

pub static PICS: Mutex<PicChain> = Mutex::new(PicChain(
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
));
