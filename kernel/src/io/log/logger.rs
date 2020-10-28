use log_crate::{Level, LevelFilter, Log, Metadata, Record};

const LOG_LEVELS: [(&str, LevelFilter); 0] = [];

struct Logger {
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

        let c = match record.level() {
            Level::Error => 'e',
            Level::Warn => 'w',
            Level::Info => 'i',
            Level::Debug => 'd',
            Level::Trace => 't',
        };

        static MAIN_LEN: usize = "syzygy_kernel ".len();
        let target = if record.target().len() >= MAIN_LEN {
            &record.target()[MAIN_LEN + 1..]
        } else {
            "init"
        };

        crate::println!("{} ({}) --> {}", c, target, record.args());
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

pub fn init() {
    log_crate::set_logger(Logger::the()).expect("Failed to initialize logging.");
    log_crate::set_max_level(LevelFilter::Trace);
}
