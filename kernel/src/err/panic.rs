use core::panic::PanicInfo;

use log_crate::error;

use crate::util::halt_loop;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    error!("{}.", info);

    halt_loop();
}
