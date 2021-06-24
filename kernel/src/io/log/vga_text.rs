//! Logging to the VGA text mode screen.

use core::fmt::{self, Write};
use core::{mem, ptr};

use crate::consts::vga_text_virt_address;
use crate::util::sync::spin::{Spinlock, SpinlockGuard};
use crate::util::sync::OnceCell;

use super::Color as LogColor;

/// A VGA color code.
#[allow(dead_code)]
#[repr(u8)]
pub(super) enum Color {
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

impl const From<u8> for Color {
    fn from(b: u8) -> Color {
        assert!(b < 16);
        unsafe { mem::transmute(b) }
    }
}

impl const Into<Color> for LogColor {
    fn into(self) -> Color {
        match self {
            Self::Gray => Color::LightGray,
            Self::Cyan => Color::Cyan,
            Self::Green => Color::Green,
            Self::LightGreen => Color::LightGreen,
            Self::Yellow => Color::Yellow,
            Self::Red => Color::Red,
        }
    }
}

/// Character colors: foreground and background.
#[derive(Copy, Clone)]
struct CharColor(u8);

impl CharColor {
    const fn new(fg: Color, bg: Color) -> CharColor {
        CharColor((bg as u8) << 4 | (fg as u8))
    }

    const fn background(&self) -> Color {
        Color::from(self.0 >> 4)
    }
}

/// A colored on-screen character.
#[allow(dead_code)]
#[derive(Copy, Clone)]
#[repr(packed)]
struct ScreenChar {
    character: u8,
    color: CharColor,
}

/// The VGA buffer.
struct ScreenBuffer([[ScreenChar; ScreenBuffer::WIDTH]; ScreenBuffer::HEIGHT]);

impl ScreenBuffer {
    const HEIGHT: usize = 25;
    const WIDTH: usize = 80;

    fn the() -> &'static mut ScreenBuffer {
        unsafe { &mut *vga_text_virt_address().as_mut_ptr() }
    }

    /// Write a character to the screen at the given position.
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

/// Utilities for writing to the VGA buffer.
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

    fn set_color(&mut self, c: Color) {
        self.color = CharColor::new(c, self.color.background());
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
#[allow(dead_code)]
pub fn _print(args: fmt::Arguments) {
    ScreenWriter::the().write_fmt(args).unwrap()
}

#[doc(hidden)]
#[allow(dead_code)]
pub fn _print_colored(c: LogColor, args: fmt::Arguments) {
    let mut writer = ScreenWriter::the();
    let old_color = writer.color;

    writer.set_color(c.into());
    writer.write_fmt(args).unwrap();
    writer.color = old_color;
}

/// Initialize the VGA buffer.
pub fn init() {
    SCREEN_WRITER.init(Spinlock::new(ScreenWriter::new(Color::Black, Color::White)));
    ScreenWriter::the().clear_screen();
}
