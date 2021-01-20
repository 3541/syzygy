//! Parsing for Stivale2.

mod info;

pub use info::StivaleInfo;

use crate::mem::{Address, VirtualAddress};

/// The header used to identify the kernel to the bootloader. See [the spec](https://github.com/stivale/stivale/blob/master/STIVALE2.md#stivale2-header-stivale2hdr) for details.
#[allow(unused)]
#[repr(packed)]
pub struct StivaleHeader {
    /// Unused: entry point is set in the linker script.
    _entry_point: u64,
    /// The address of the initial stack.
    stack: *const u8,
    /// Currently unused.
    _flags: u64,
    /// A linked list of tags.
    tags: VirtualAddress,
}

// Required to be used in static context. Invariants preserved since mutation is
// forbidden.
unsafe impl Send for StivaleHeader {}
unsafe impl Sync for StivaleHeader {}

impl StivaleHeader {
    /// # Safety
    /// The given pointer must refer to a sufficiently large area of memory in the bss section.
    pub const unsafe fn new(stack: *const u8) -> StivaleHeader {
        StivaleHeader {
            _entry_point: 0,
            stack,
            _flags: 0,
            tags: VirtualAddress::new_unchecked(0),
        }
    }
}
