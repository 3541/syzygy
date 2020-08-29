use core::fmt::{self, Write};

use lazy_static::lazy_static;
use rgb::RGB8;
use volatile::Volatile;

use crate::arch::port::Port;
use crate::constants::KERNEL_BASE;
use crate::interrupt::without_interrupts;
use crate::memory::PhysicalAddress;
use crate::sync::SpinLock;

pub const VGA_BUFFER_ADDRESS: PhysicalAddress = Buffer::ADDRESS;
const VGA_INPUT_STATUS: u16 = 0x3DA;
const VGA_ADDRESS_DATA: u16 = 0x3C0;
const VGA_ADDRESS_DATA_READ: u16 = 0x3C1;

#[derive(Copy, Clone)]
#[repr(u8)]
#[allow(dead_code)]
pub enum Color {
    Black = 0,
    Blue = 1,
    Green = 2,
    Cyan = 3,
    Red = 4,
    Magenta = 5,
    Brown = 6,
    LightGray = 7,
    DarkGray = 8,
    LightBlue = 9,
    LightGreen = 10,
    LightCyan = 11,
    LightRed = 12,
    Pink = 13,
    Yellow = 14,
    White = 15,
}

impl Into<RGB8> for Color {
    fn into(self) -> RGB8 {
        match self {
            Color::Black => ansi_rgb::black(),
            Color::Blue => ansi_rgb::blue(),
            Color::Green => ansi_rgb::green(),
            Color::Cyan => ansi_rgb::cyan(),
            Color::Red => ansi_rgb::red(),
            Color::Magenta => ansi_rgb::magenta(),
            Color::Brown => RGB8::new(139, 69, 19),
            Color::LightGray => RGB8::new(98, 98, 98),
            Color::DarkGray => RGB8::new(200, 200, 200),
            Color::LightBlue => ansi_rgb::cyan_blue(),
            Color::LightGreen => ansi_rgb::yellow_green(),
            Color::LightCyan => RGB8::new(224, 255, 255),
            Color::LightRed => RGB8::new(255, 99, 71),
            Color::Pink => RGB8::new(255, 192, 203),
            Color::Yellow => ansi_rgb::yellow(),
            Color::White => ansi_rgb::white(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ColorCode(u8);

impl ColorCode {
    const fn new(fg: Color, bg: Color) -> Self {
        ColorCode((bg as u8) << 4 | (fg as u8))
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(C, packed)]
struct VgaChar {
    character: u8,
    color: ColorCode,
}

struct Buffer {
    characters: [[Volatile<VgaChar>; Buffer::WIDTH]; Buffer::HEIGHT],
}

impl Buffer {
    const ADDRESS: PhysicalAddress = unsafe { PhysicalAddress::new_const(0xB8000) };
    const HEIGHT: usize = 25;
    const WIDTH: usize = 80;
}

lazy_static! {
    pub static ref WRITER: SpinLock<Writer> = {
        // First disable blink.
        unsafe {
            // Go into index state
            Port::<u8>::new(VGA_INPUT_STATUS).read();

            let mut addr_data: Port<u8> = Port::new(VGA_ADDRESS_DATA);
            // Select the attribute mode control register
            addr_data.write(0x10 | (1 << 5));
            // Read that register
            let reg = Port::<u8>::new(VGA_ADDRESS_DATA_READ).read();

            // Disable blink
            addr_data.write(reg & !(1 << 3));
        }

        SpinLock::new(Writer {
            column: 0,
            #[cfg(not(feature = "integration-tests"))]
            color: ColorCode::new(Color::Black, Color::White),
            #[cfg(feature = "integration-tests")]
            color: ColorCode::new(Color::Green, Color::Black),
            buffer: unsafe { &mut *(*(Buffer::ADDRESS + *KERNEL_BASE) as *mut Buffer) },
        })
    };
}

pub struct Writer {
    column: usize,
    color: ColorCode,
    buffer: &'static mut Buffer,
}

impl Writer {
    pub fn color(&self) -> ColorCode {
        self.color
    }

    pub fn set_color(&mut self, fg: Color, bg: Color) {
        self.color = ColorCode::new(fg, bg);
    }

    pub fn set_color_code(&mut self, c: ColorCode) {
        self.color = c;
    }

    fn new_line(&mut self) {
        for row in 1..Buffer::HEIGHT {
            for column in 0..Buffer::WIDTH {
                let character = self.buffer.characters[row][column].read();
                self.buffer.characters[row - 1][column].write(character);
            }
        }
        self.clear_row(Buffer::HEIGHT - 1);
        self.column = 0;
    }

    fn clear_row(&mut self, row: usize) {
        let space = VgaChar {
            character: b' ',
            color: self.color,
        };

        for column in 0..Buffer::WIDTH {
            self.buffer.characters[row][column].write(space);
        }
    }

    pub fn clear_screen(&mut self) {
        for row in 0..Buffer::HEIGHT {
            self.clear_row(row)
        }
    }

    pub fn write_byte(&mut self, byte: u8) {
        match byte {
            b'\n' => self.new_line(),
            byte => {
                if self.column >= Buffer::WIDTH {
                    self.new_line();
                }

                let c = self.column;
                let cl = self.color;
                self.buffer.characters[Buffer::HEIGHT - 1][c].write(VgaChar {
                    character: byte,
                    color: cl,
                });
                self.column += 1;
            }
        }
    }
}

impl Write for Writer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for b in s.bytes() {
            self.write_byte(b)
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! vga_print {
    ($($arg:tt)*) => ($crate::vga_text::_print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! vga_println {
    () => ($crate::vga_print!('\n'));
    ($($arg:tt)*) => ($crate::vga_print!("{}\n", format_args!($($arg)*)));
}

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    without_interrupts(|| WRITER.lock().write_fmt(args).unwrap())
}

#[cfg(test)]
mod test {
    use array_init::array_init;

    use super::*;

    fn create_writer() -> Writer {
        Writer {
            column: 0,
            color: ColorCode::new(Color::Black, Color::White),
            buffer: Box::leak(Box::new(create_buffer())),
        }
    }

    fn create_buffer() -> Buffer {
        Buffer {
            characters: array_init(|_| {
                array_init(|_| {
                    Volatile::new(VgaChar {
                        character: b' ',
                        color: ColorCode::new(Color::Black, Color::White),
                    })
                })
            }),
        }
    }

    #[test]
    fn write_byte() {
        let mut writer = create_writer();
        writer.write_byte(b'S');
        writer.write_byte(b'Z');

        for (i, row) in writer.buffer.characters.iter().enumerate() {
            for (j, c) in row.iter().enumerate() {
                let c = c.read();
                assert_eq!(c.color, writer.color);
                if i == Buffer::HEIGHT - 1 {
                    if j == 0 {
                        assert_eq!(c.character, b'S');
                    } else if j == 1 {
                        assert_eq!(c.character, b'Z');
                    }
                } else {
                    assert_eq!(
                        c,
                        VgaChar {
                            character: b' ',
                            color: ColorCode::new(Color::Black, Color::White)
                        }
                    );
                }
            }
        }
    }
}
