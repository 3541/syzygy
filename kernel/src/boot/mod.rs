// Architecture-specific submodules should define the entry point kinit, which
// in turn should call kmain after performing preliminary architecture-specific
// initialization tasks.
mod arch;

use log_crate::info;

use crate::io::log;
use crate::{consts, int, util};

fn kmain() {
    log::init();
    info!("kmain.");

    info!("This is {}, version {}.", consts::NAME, consts::VERSION);

    int::init();
    info!("INITIALIZED interrupts.");

    util::halt_loop();
}
