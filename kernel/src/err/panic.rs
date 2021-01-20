//! The panic handler

use core::panic::PanicInfo;

use log_crate::error;

use crate::util::halt_loop;

/// The panic handler. Prints diagnostics and halts.
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    error!("{}.", info);

    halt_loop();
}
