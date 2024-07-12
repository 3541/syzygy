/*
 * LOG
 *
 * Copyright (c) 2020, 2024 Alex O'Brien <3541@3541.website>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of
 * the MPL was not distributed with this file, you can obtain one at http://mozilla.org/MPL/2.0/.
 */

mod arch;

use core::fmt;

use log::{Level, LevelFilter, Log, Metadata, Record};

#[derive(Copy, Clone)]
pub enum Color {
    Gray,
    Cyan,
    Green,
    LightGreen,
    Yellow,
    Red,
}

#[doc(hidden)]
pub fn print(args: fmt::Arguments) {
    #[cfg(target_arch = "x86_64")]
    arch::e9::print(args);
}

#[doc(hidden)]
pub fn print_colored(color: Color, args: fmt::Arguments) {
    #[cfg(target_arch = "x86_64")]
    arch::e9::print_colored(color, args);
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::io::log::print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! print_colored {
    ($c:expr, $($arg:tt)*) => ($crate::io::log::print_colored($c, format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!('\n'));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println_colored {
    ($c:expr, $($arg:tt)*) => ($crate::print_colored!($c, "{}\n", format_args!($($arg)*)));
}

struct Logger {
    level: LevelFilter,
}

impl Logger {
    fn the() -> &'static Self {
        static INSTANCE: Logger = Logger {
            level: LevelFilter::Debug,
        };

        &INSTANCE
    }
}

impl Log for Logger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= self.level
    }

    fn log(&self, record: &Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        let (prefix, color) = match record.level() {
            Level::Error => ("E", Color::Red),
            Level::Warn => ("W", Color::Yellow),
            Level::Info => ("I", Color::Green),
            Level::Debug => ("D", Color::Cyan),
            Level::Trace => ("T", Color::Gray),
        };

        print_colored!(color, "<{}> ", prefix);
        println!("[{}]: {}", record.target(), record.args());
    }

    fn flush(&self) {}
}

pub fn init() {
    log::set_logger(Logger::the()).expect("Failed to initialize logging.");
    log::set_max_level(LevelFilter::Trace);
}
