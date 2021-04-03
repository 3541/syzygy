//! Logging facilities.

use core::fmt;

mod arch;
mod logger;
mod vga_text;

pub use arch::*;

#[derive(Copy, Clone)]
#[allow(dead_code)]
pub enum Color {
    Gray,
    Cyan,
    Green,
    LightGreen,
    Yellow,
    Red,
}

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    #[cfg(feature = "log_vga")]
    vga_text::_print(args);
    #[cfg(feature = "log_e9")]
    e9::_print(args);
}

#[doc(hidden)]
pub fn _print_colored(color: Color, args: fmt::Arguments) {
    #[cfg(feature = "log_vga")]
    vga_text::_print_colored(color, args);
    #[cfg(feature = "log_e9")]
    e9::_print_colored(color, args);
}

/// Print to all log sinks.
#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::io::log::_print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! print_colored {
    ($c:expr, $($arg:tt)*) => ($crate::io::log::_print_colored($c, format_args!($($arg)*)));
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

/// Initialize logging and all available log sinks.
pub fn init() {
    vga_text::init();
    logger::init();
}
