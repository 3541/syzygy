use core::fmt::Write;

use ansi_rgb::Foreground;
use logc::{Level, LevelFilter, Metadata, Record};

use crate::interrupt::without_interrupts;
use crate::vga_text::{self, Color};
use crate::{serial_print, serial_println};

const LOG_MODULE_LEVELS: [(&'static str, LevelFilter); 1] =
    [("syzygy::memory::paging", LevelFilter::Debug)];

struct Log {
    default_level: LevelFilter,
}

impl logc::Log for Log {
    #[cfg(debug_assertions)]
    #[inline]
    fn enabled(&self, metadata: &Metadata) -> bool {
        let message_level = metadata.level();
        if message_level <= self.default_level {
            true
        } else {
            for (target, level) in LOG_MODULE_LEVELS.iter() {
                if metadata.target().starts_with(target) {
                    return message_level <= *level;
                }
            }
            false
        }
    }

    #[cfg(not(debug_assertions))]
    #[inline]
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            without_interrupts(|| {
                let mut writer = vga_text::WRITER.lock();
                let prev_color = writer.color();
                let color = match record.level() {
                    Level::Trace => Color::LightGray,
                    Level::Debug => Color::Cyan,
                    Level::Info => Color::LightGreen,
                    Level::Warn => Color::Yellow,
                    Level::Error => Color::Red,
                };
                writer.set_color(color, Color::White);
                write!(*writer, "{}", record.level()).unwrap();
                serial_print!("{}", record.level().fg(color.into()));
                writer.set_color_code(prev_color);
                writeln!(*writer, " ({}): {}", record.target(), record.args()).unwrap();
                serial_println!(" ({}): {}", record.target(), record.args());
            })
        }
    }

    fn flush(&self) {}
}

static LOG: Log = Log {
    #[cfg(debug_assertions)]
    default_level: LevelFilter::Debug,
    #[cfg(not(debug_assertions))]
    default_level: LevelFilter::Info,
};

#[cfg(debug_assertions)]
const COMPILE_LEVEL_FILTER: LevelFilter = LevelFilter::Trace;

#[cfg(not(debug_assertions))]
const COMPILE_LEVEL_FILTER: LevelFilter = LevelFilter::Info;

pub fn init() {
    logc::set_logger(&LOG)
        .map(|()| logc::set_max_level(COMPILE_LEVEL_FILTER))
        .expect("Failed to init log.");
}
