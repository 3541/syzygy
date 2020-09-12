// Global constants, mostly from the linker script.

use crate::mem::VirtualAddress;

#[allow(dead_code)]
extern "C" {
    static SZ_KERNEL_START: u8;
}

pub static KERNEL_START: VirtualAddress =
    unsafe { VirtualAddress::new_const(0xFFFF_C000_0000_0000) };
//    unsafe { VirtualAddress::new_const(&SZ_KERNEL_START as *const _ as RawVirtualAddress) }; -- not currently allowed.
