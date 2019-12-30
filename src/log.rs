use core::fmt::Write;

use ansi_rgb::Foreground;
use logc::{Level, LevelFilter, Metadata, Record};

use crate::vga_text::{self, Color};
use crate::{serial_print, serial_println};

struct Log {
    level: LevelFilter,
}

impl logc::Log for Log {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= self.level
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let mut writer = vga_text::WRITER.lock();
            let prev_color = writer.color();
            let color = match record.level() {
                Level::Debug | Level::Trace => Color::Cyan,
                Level::Info => Color::Black,
                Level::Warn => Color::Yellow,
                Level::Error => Color::Red,
            };
            writer.set_color(color, Color::White);
            write!(*writer, "{}", record.level()).unwrap();
            serial_print!("{}", record.level().fg(color.into()));
            writer.set_color_code(prev_color);
            writeln!(*writer, ": {}", record.args()).unwrap();
            serial_println!(": {}", record.args());
        }
    }

    fn flush(&self) {}
}

static LOG: Log = Log {
    level: LevelFilter::Trace,
};

pub fn init() {
    logc::set_logger(&LOG)
        .map(|()| logc::set_max_level(LOG.level))
        .expect("Failed to init log.");
}
