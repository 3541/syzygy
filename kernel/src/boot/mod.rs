// Architecture-specific submodules should define the entry point kinit, which
// in turn should call kmain after performing preliminary architecture-specific
// initialization tasks.
mod arch;

use log_crate::info;

use crate::mem::map::{Mmap, MmapEntry};
use crate::{consts, int, util};

fn kmain(mmap: Mmap<impl Iterator<Item = MmapEntry>>) {
    info!("kmain.");

    info!("This is {}, version {}.", consts::NAME, consts::VERSION);

    int::init();
    info!("INITIALIZED interrupts.");

    util::halt_loop();
}
