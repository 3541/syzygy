use crate::util::spin::OnceCell;
use core::ptr;

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
        unsafe { &mut *(crate::consts::KERNEL_START + 0xB8000).as_mut_ptr() }
    }

    fn write(&mut self, row: usize, col: usize, value: ScreenChar) {
        if row >= ScreenBuffer::HEIGHT || col >= ScreenBuffer::WIDTH {
            panic!("ScreenBuffer index ({}, {}) out of bounds", row, col);
        }

        unsafe { ptr::write_volatile(&mut self.0[row][col], value) };
    }
}

struct ScreenWriter {
    column: usize,
    color: CharColor,
    buffer: &'static mut ScreenBuffer,
}

static SCREEN_WRITER: OnceCell<ScreenWriter> = OnceCell::new();

impl ScreenWriter {
    fn new(fg: Color, bg: Color) -> ScreenWriter {
        ScreenWriter {
            column: 0,
            color: CharColor::new(fg, bg),
            buffer: ScreenBuffer::the(),
        }
    }

    //  Todo: Mutex
    fn the() -> MutexGuard<ScreenWriter>;
}

pub fn init() {
    SCREEN_WRITER.init(ScreenWriter::new(Color::White, Color::Black));
}
