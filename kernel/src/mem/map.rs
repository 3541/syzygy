//! The memory map.

use alloc::vec::Vec;
use core::fmt::{self, Display};

use super::PhysicalAddress;

/// Memory region types.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MmapEntryType {
    /// Free to be allocated.
    Usable,
    /// Owned by hardware or otherwise inaccessible.
    Reserved,
    /// Occupied by the kernel.
    Kernel,
}

/// An entry in the memory map.
#[derive(Debug, Copy, Clone)]
pub struct MmapEntry {
    /// The type of the described region.
    pub entry_type: MmapEntryType,
    /// The region's address.
    pub start: PhysicalAddress,
    /// The region's size.
    pub size: usize,
}

impl Display for MmapEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} - {}: {:?}",
            self.start,
            self.start + self.size,
            self.entry_type
        )
    }
}

/// The memory map.
pub type Mmap = Vec<MmapEntry>;
