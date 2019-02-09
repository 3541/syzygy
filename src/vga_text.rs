use core::fmt::{self, Write};

use lazy_static::lazy_static;
use spin::Mutex;
use volatile::Volatile;

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

#[derive(Copy, Clone)]
struct ColorCode(u8);

impl ColorCode {
    const fn new(fg: Color, bg: Color) -> Self {
        ColorCode((bg as u8) << 4 | (fg as u8))
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
struct VgaChar {
    character: u8,
    color: ColorCode,
}

const BUFFER_HEIGHT: usize = 25;
const BUFFER_WIDTH: usize = 80;

struct Buffer {
    characters: [[Volatile<VgaChar>; BUFFER_WIDTH]; BUFFER_HEIGHT],
}

lazy_static! {
    pub static ref WRITER: Mutex<Writer> = Mutex::new(Writer {
        column: 0,
        color: ColorCode::new(Color::Black, Color::White),
        buffer: unsafe { &mut *((crate::KERNEL_BASE + 0xB8000) as *mut Buffer) },
    });
}

pub struct Writer {
    column: usize,
    color: ColorCode,
    buffer: &'static mut Buffer,
}

impl Writer {
    fn new_line(&mut self) {
        for row in 1..BUFFER_HEIGHT {
            for column in 0..BUFFER_WIDTH {
                let character = self.buffer.characters[row][column].read();
                self.buffer.characters[row - 1][column].write(character);
            }
        }
        self.clear_row(BUFFER_HEIGHT - 1);
        self.column = 0;
    }

    fn clear_row(&mut self, row: usize) {
        let space = VgaChar {
            character: b' ',
            color: self.color,
        };

        for column in 0..BUFFER_WIDTH {
            self.buffer.characters[row][column].write(space);
        }
    }

    pub fn clear_screen(&mut self) {
        for row in 0..BUFFER_HEIGHT {
            self.clear_row(row)
        }
    }

    pub fn write_byte(&mut self, byte: u8) {
        match byte {
            b'\n' => self.new_line(),
            byte => {
                if self.column >= BUFFER_WIDTH {
                    self.new_line();
                }

                let c = self.column;
                let cl = self.color;
                self.buffer.characters[BUFFER_HEIGHT - 1][c].write(VgaChar {
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
    WRITER.lock().write_fmt(args).unwrap()
}
