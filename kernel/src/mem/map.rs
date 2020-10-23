use alloc::vec::Vec;
use core::fmt::{self, Display};

use super::PhysicalAddress;

// Memory map used for initializing memory management.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MmapEntryType {
    Usable,
    Reserved,
    Kernel,
}

#[derive(Debug, Copy, Clone)]
pub struct MmapEntry {
    pub entry_type: MmapEntryType,
    pub start: PhysicalAddress,
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

pub type Mmap = Vec<MmapEntry>;
