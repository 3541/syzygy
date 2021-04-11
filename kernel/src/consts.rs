/*!

# Global constants.
Mostly pulled from the linker script.

!*/

use crate::mem::{Address, VirtualAddress};

pub mod image {
    use crate::mem::{Address, VirtualAddress, VirtualRange};

    #[allow(dead_code)]
    extern "C" {
        /// Linker script symbol for the start of the kernel image.
        static SZ_KERNEL_BASE: u8;
        static SZ_KERNEL_TEXT_BASE: u8;
        static SZ_KERNEL_TEXT_END: u8;
        static SZ_KERNEL_DATA_BASE: u8;
        static SZ_KERNEL_DATA_END: u8;
        static SZ_KERNEL_BSS_BASE: u8;
        static SZ_KERNEL_BSS_END: u8;
        static SZ_KERNEL_RODATA_BASE: u8;
        static SZ_KERNEL_RODATA_END: u8;
        static SZ_KERNEL_TDATA_BASE: u8;
        static SZ_KERNEL_TDATA_END: u8;
        static SZ_KERNEL_TBSS_BASE: u8;
        static SZ_KERNEL_TBSS_END: u8;
    }

    pub const LOAD_BASE: VirtualAddress =
        unsafe { VirtualAddress::new_unchecked(0xFFFFFFFF80000000) };

    /// The virtual address at which the kernel image begins.
    pub fn base() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_BASE) }
    }

    pub fn text_base() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_TEXT_BASE) }
    }

    pub fn text_end() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_TEXT_END) }
    }

    pub fn text_range() -> VirtualRange {
        VirtualRange::new(text_base(), text_end() - text_base())
    }

    pub fn data_base() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_DATA_BASE) }
    }

    pub fn data_end() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_DATA_END) }
    }

    pub fn data_range() -> VirtualRange {
        VirtualRange::new(data_base(), data_end() - data_base())
    }

    pub fn bss_base() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_BSS_BASE) }
    }

    pub fn bss_end() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_BSS_END) }
    }

    pub fn bss_range() -> VirtualRange {
        VirtualRange::new(bss_base(), bss_end() - bss_base())
    }

    pub fn rodata_base() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_RODATA_BASE) }
    }

    pub fn rodata_end() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_RODATA_END) }
    }

    pub fn rodata_range() -> VirtualRange {
        VirtualRange::new(rodata_base(), rodata_end() - rodata_base())
    }

    pub fn tdata_base() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_TDATA_BASE) }
    }

    pub fn tdata_end() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_TDATA_END) }
    }

    pub fn tdata_range() -> VirtualRange {
        VirtualRange::new(tdata_base(), tdata_end() - tdata_base())
    }

    pub fn tbss_base() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_TBSS_BASE) }
    }

    pub fn tbss_end() -> VirtualAddress {
        unsafe { VirtualAddress::from_ptr_unchecked(&SZ_KERNEL_TBSS_END) }
    }

    pub fn tbss_range() -> VirtualRange {
        VirtualRange::new(tbss_base(), tbss_end() - tbss_base())
    }
}

/// 4G of physical memory is mapped here. See
/// [the Stivale 2 documentation](https://github.com/stivale/stivale/blob/master/STIVALE2.md#64-bit-kernel).
pub static PHYS_BASE: VirtualAddress =
    unsafe { VirtualAddress::new_unchecked(0xFFFF_8000_0000_0000) };

/// The package name. This is probably `syzygy_kernel`.
pub const NAME: &str = env!("CARGO_PKG_NAME");
/// The current version and git revision.
pub const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), "+", env!("GIT_HASH"));
