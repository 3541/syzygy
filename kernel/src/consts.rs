// Global constants, mostly from the linker script.

use crate::mem::VirtualAddress;

#[allow(dead_code)]
extern "C" {
    static SZ_KERNEL_START: u8;
}

// Kernel is loaded here.
pub static KERNEL_START: VirtualAddress =
    unsafe { VirtualAddress::new_const(0xFFFF_FFFF_8000_0000) };

// 4G of physical memory is mapped here.
pub static PHYS_BASE: VirtualAddress = unsafe { VirtualAddress::new_const(0xFFFF_8000_0000_0000) };

pub const NAME: &str = env!("CARGO_PKG_NAME");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
