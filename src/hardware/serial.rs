use core::fmt;

use bitflags::bitflags;
use lazy_static::lazy_static;
use spin::Mutex;

use crate::hardware::port::Port;

const COM1: u16 = 0x3F8;

pub struct SerialPort {
    data: Port<u8>,
    interrupt_enable: Port<u8>,
    fifo_control: Port<u8>,
    line_control: Port<u8>,
    modem_control: Port<u8>,
    line_status: Port<u8>,
}

lazy_static! {
    pub static ref SERIAL1: Mutex<SerialPort> = {
        Mutex::new({
            let mut s = SerialPort::new(COM1);
            s.init();
            s
        })
    };
}

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    use fmt::Write;
    SERIAL1
        .lock()
        .write_fmt(args)
        .expect("Failed to print to serial.")
}

#[macro_export]
macro_rules! serial_print {
    ($($arg:tt)*) => {
        $crate::hardware::serial::_print(format_args!($($arg)*));
    };
}

#[macro_export]
macro_rules! serial_println {
    () => ($crate::serial_print!('\n'));
    ($($arg:tt)*) => ($crate::serial_print!("{}\n", format_args!($($arg)*)));
}

bitflags! {
    struct InterruptEnableRegisterFlags: u8 {
        const DATA_AVAILABLE = 1;
        const TRANSMITTER_EMPTY = 1 << 1;
        const ERROR = 1 << 2;
        const STATUS_CHANGE = 1 << 3;
    }
}

bitflags! {
    struct LineStatusRegisterFlags: u8 {
        const DATA_READY = 1;
        const TRANSMITTER_EMPTY = 1 << 5;
    }
}

bitflags! {
    struct LineControlRegisterFlags: u8 {
        const WORDL_8 = 0b11;
        const DLAB = 1 << 7;
    }
}

bitflags! {
    struct FifoControlRegisterFlags: u8 {
        const ENABLE_FIFO = 1;
        const CLEAR_RX_FIFO = 1 << 1;
        const CLEAR_TX_FIFO = 1 << 2;
        const INTERRUPT_TRIGGER_14_BYTES = (1 << 6) | (1 << 7);
    }
}

bitflags! {
    struct ModemControlRegisterFlags: u8 {
        const DATA_TERMINAL_READY = 1;
        const REQUEST_TO_SEND = 1 << 1;
        const AUX_OUTPUT_2 = 1 << 3;
    }
}

impl SerialPort {
    pub const fn new(address: u16) -> Self {
        SerialPort {
            data: Port::new(address),
            interrupt_enable: Port::new(address + 1),
            fifo_control: Port::new(address + 2),
            line_control: Port::new(address + 3),
            modem_control: Port::new(address + 4),
            line_status: Port::new(address + 5),
        }
    }

    pub fn init(&mut self) {
        unsafe {
            self.interrupt_enable.write(0);
            self.line_control.write(LineControlRegisterFlags::DLAB.bits); // enable DLAB
            self.data.write(3); // 38400 baud
            self.interrupt_enable.write(0); // high byte
            self.line_control
                .write(LineControlRegisterFlags::WORDL_8.bits); // disable DLAB, set 8N1
            self.fifo_control.write(
                (FifoControlRegisterFlags::ENABLE_FIFO
                    | FifoControlRegisterFlags::CLEAR_RX_FIFO
                    | FifoControlRegisterFlags::CLEAR_TX_FIFO
                    | FifoControlRegisterFlags::INTERRUPT_TRIGGER_14_BYTES)
                    .bits,
            ); // enable FIFO
            self.modem_control.write(
                (ModemControlRegisterFlags::DATA_TERMINAL_READY
                    | ModemControlRegisterFlags::REQUEST_TO_SEND
                    | ModemControlRegisterFlags::AUX_OUTPUT_2)
                    .bits,
            ); // enable IRQs, DTR/RTS
        }
    }

    fn line_status(&self) -> LineStatusRegisterFlags {
        LineStatusRegisterFlags::from_bits_truncate(unsafe { self.line_status.read() })
    }

    pub fn send_byte(&mut self, byte: u8) {
        while !self
            .line_status()
            .contains(LineStatusRegisterFlags::TRANSMITTER_EMPTY)
        {}
        unsafe { self.data.write(byte) }
    }

    pub fn recv_byte(&mut self) -> Option<u8> {
        if self
            .line_status()
            .contains(LineStatusRegisterFlags::DATA_READY)
        {
            Some(unsafe { self.data.read() })
        } else {
            None
        }
    }
}

impl fmt::Write for SerialPort {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for byte in s.bytes() {
            self.send_byte(byte)
        }
        Ok(())
    }
}
