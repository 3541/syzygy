/*!

# System initialization.

Architecture-specific submodules should define the entry point kinit, which
in turn should call kmain after performing preliminary architecture-specific
initialization tasks.

!*/

mod arch;

use log_crate::info;

use crate::mem::map::Mmap;
use crate::{consts, int, mem, task};

/// Architecture-independent kernel entry point. This is called from [kinit](arch::kinit).
fn kmain(slide: usize, mmap: Mmap) {
    info!("kmain.");

    info!("This is {}, version {}.", consts::NAME, consts::VERSION);

    int::init();
    info!("INITIALIZED interrupts.");

    mem::phys::init(&mmap);
    info!("INITIALIZED physical memory management.");

    mem::virt::init(slide);
    info!("INITIALIZED virtual memory management.");

    task::init(kmain2());
}

async fn kmain2() {
    info!("INITIALIZED early multitasking.");
}
