/*!

# System initialization.

Architecture-specific submodules should define the entry point kinit, which
in turn should call kmain after performing preliminary architecture-specific
initialization tasks.

!*/

mod arch;

use log_crate::info;

use crate::mem::map::Mmap;
use crate::{consts, int, mem, util};

/// Architecture-independent kernel entry point. This is called from [kinit](arch::kinit).
fn kmain(mmap: Mmap) {
    info!("kmain.");

    info!("This is {}, version {}.", consts::NAME, consts::VERSION);

    int::init();
    info!("INITIALIZED interrupts.");

    mem::phys::init(&mmap);
    info!("INITIALIZED physical memory management.");

    util::halt_loop();
}
