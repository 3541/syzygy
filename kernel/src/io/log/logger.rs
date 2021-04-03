//! Log formatting.

use log_crate::{Level, LevelFilter, Log, Metadata, Record};

use super::Color;

/// Per-module log level overrides.
const LOG_LEVELS: [(&str, LevelFilter); 0] = [];

/// The global logger.
struct Logger {
    /// The current global level filter.
    level: LevelFilter,
}

impl Log for Logger {
    #[inline]
    fn enabled(&self, metadata: &Metadata) -> bool {
        let level = metadata.level();
        if level <= self.level {
            true
        } else {
            for (target, filter) in LOG_LEVELS.iter() {
                if metadata.target().starts_with(target) {
                    return level <= *filter;
                }
            }

            false
        }
    }

    fn log(&self, record: &Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        let (prefix, color) = match record.level() {
            Level::Error => ('e', Color::Red),
            Level::Warn => ('w', Color::Yellow),
            Level::Info => ('i', Color::Green),
            Level::Debug => ('d', Color::Cyan),
            Level::Trace => ('t', Color::Gray),
        };

        static MAIN_LEN: usize = "syzygy_kernel ".len();
        let target = if record.target().len() >= MAIN_LEN {
            &record.target()[MAIN_LEN + 1..]
        } else {
            "init"
        };

        crate::print_colored!(color, "{} ", prefix);
        crate::println!("({}) --> {}", target, record.args());
    }

    fn flush(&self) {
        // no-op
    }
}

impl Logger {
    fn the() -> &'static Logger {
        static LOGGER: Logger = Logger {
            level: LevelFilter::Trace,
        };

        &LOGGER
    }
}

/// Initialize the logger.
pub fn init() {
    log_crate::set_logger(Logger::the()).expect("Failed to initialize logging.");
    log_crate::set_max_level(LevelFilter::Trace);
}
