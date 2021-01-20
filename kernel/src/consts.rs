/*!

# Global constants.
Mostly pulled from the linker script.

!*/

use crate::mem::{Address, VirtualAddress};

#[allow(dead_code)]
extern "C" {
    /// Linker script symbol for the start of the kernel image.
    static SZ_KERNEL_BASE: u8;
}

/// The virtual address at which the kernel image begins.
pub fn kernel_base() -> VirtualAddress {
    unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_BASE) }
}

/// 4G of physical memory is mapped here. See
/// [the Stivale 2 documentation](https://github.com/stivale/stivale/blob/master/STIVALE2.md#64-bit-kernel).
pub static PHYS_BASE: VirtualAddress =
    unsafe { VirtualAddress::new_unchecked(0xFFFF_8000_0000_0000) };

/// The package name. This is probably `syzygy_kernel`.
pub const NAME: &str = env!("CARGO_PKG_NAME");
/// The current version and git revision.
pub const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), "+", env!("GIT_HASH"));
