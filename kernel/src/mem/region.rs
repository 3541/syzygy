//! A virtual memory region.

use super::VirtualAddress;

/// A virtual memory region.
#[derive(Copy, Clone)]
pub struct VirtualRegion {
    /// The region's base address.
    start: VirtualAddress,
    /// The size of the region.
    size: usize,
}

impl VirtualRegion {
    /// Create a new region.
    pub const fn new(start: VirtualAddress, size: usize) -> VirtualRegion {
        VirtualRegion { start, size }
    }
}
