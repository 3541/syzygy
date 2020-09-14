// Logging facilities. All log output is printed to all enabled logging sinks.

use core::fmt;

mod arch;
mod logger;
mod vga_text;

pub use arch::*;

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    if cfg!(feature = "log_vga") {
        vga_text::_print(args);
    }
    if cfg!(feature = "log_e9") {
        e9::_print(args);
    }
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::io::log::_print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!('\n'));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

pub fn init() {
    vga_text::init();

    logger::init();
}
