//! Logging facilities.

use core::fmt;

mod arch;
mod logger;
mod vga_text;

pub use arch::*;

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    #[cfg(feature = "log_vga")]
    vga_text::_print(args);
    #[cfg(feature = "log_e9")]
    e9::_print(args);
}

/// Print to all log sinks.
#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::io::log::_print(format_args!($($arg)*)));
}

/// Print to all log sinks.
#[macro_export]
macro_rules! println {
    () => ($crate::print!('\n'));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

/// Initialize logging and all available log sinks.
pub fn init() {
    vga_text::init();
    logger::init();
}
