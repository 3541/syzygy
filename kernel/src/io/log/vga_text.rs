use core::fmt::{self, Write};
use core::ptr;

use crate::util::sync::spin::{Spinlock, SpinlockGuard};
use crate::util::sync::OnceCell;

#[allow(dead_code)]
#[repr(u8)]
enum Color {
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
struct CharColor(u8);

impl CharColor {
    const fn new(fg: Color, bg: Color) -> CharColor {
        CharColor((bg as u8) << 4 | (fg as u8))
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone)]
#[repr(packed)]
struct ScreenChar {
    character: u8,
    color: CharColor,
}

struct ScreenBuffer([[ScreenChar; ScreenBuffer::WIDTH]; ScreenBuffer::HEIGHT]);

impl ScreenBuffer {
    const HEIGHT: usize = 25;
    const WIDTH: usize = 80;

    fn the() -> &'static mut ScreenBuffer {
        unsafe { &mut *(crate::consts::PHYS_BASE + 0xB8000).as_mut_ptr() }
    }

    fn write(&mut self, row: usize, col: usize, value: ScreenChar) {
        if row >= ScreenBuffer::HEIGHT || col >= ScreenBuffer::WIDTH {
            panic!("ScreenBuffer index ({}, {}) out of bounds", row, col);
        }

        unsafe { ptr::write_volatile(&mut self.0[row][col], value) };
    }

    fn read(&self, row: usize, col: usize) -> ScreenChar {
        self.0[row][col]
    }
}

struct ScreenWriter {
    column: usize,
    color: CharColor,
    buffer: &'static mut ScreenBuffer,
}

static SCREEN_WRITER: OnceCell<Spinlock<ScreenWriter>> = OnceCell::new();

impl ScreenWriter {
    fn new(fg: Color, bg: Color) -> ScreenWriter {
        ScreenWriter {
            column: 0,
            color: CharColor::new(fg, bg),
            buffer: ScreenBuffer::the(),
        }
    }

    fn the() -> SpinlockGuard<'static, ScreenWriter> {
        SCREEN_WRITER.lock()
    }

    fn clear_row(&mut self, row: usize) {
        for col in 0..ScreenBuffer::WIDTH {
            self.buffer.write(
                row,
                col,
                ScreenChar {
                    character: b' ',
                    color: self.color,
                },
            );
        }
    }

    pub fn clear_screen(&mut self) {
        for row in 0..ScreenBuffer::HEIGHT {
            self.clear_row(row);
        }
    }

    fn new_line(&mut self) {
        for row in 1..ScreenBuffer::HEIGHT {
            for col in 0..ScreenBuffer::WIDTH {
                self.buffer.write(row - 1, col, self.buffer.read(row, col));
            }
        }
        self.clear_row(ScreenBuffer::HEIGHT - 1);
        self.column = 0;
    }

    pub fn write_byte(&mut self, byte: u8) {
        if byte == b'\n' {
            self.new_line()
        } else {
            if self.column >= ScreenBuffer::WIDTH {
                self.new_line();
            }

            self.buffer.write(
                ScreenBuffer::HEIGHT - 1,
                self.column,
                ScreenChar {
                    character: byte,
                    color: self.color,
                },
            );
            self.column += 1;
        }
    }
}

impl Write for ScreenWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for b in s.bytes() {
            self.write_byte(b);
        }
        Ok(())
    }
}

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    ScreenWriter::the().write_fmt(args).unwrap()
}

pub fn init() {
    SCREEN_WRITER.init(Spinlock::new(ScreenWriter::new(Color::White, Color::Black)));
    ScreenWriter::the().clear_screen();
}
