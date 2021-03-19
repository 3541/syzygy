//! A virtual memory region.

use alloc::vec::Vec;
use core::iter::IntoIterator;

use super::{Page, VirtualAddress};

/// A virtual address range.
pub struct VirtualRange {
    /// The range's base address.
    start: VirtualAddress,
    /// The size of the range.
    size: usize,
}

pub struct VirtualRangeIter {
    current: VirtualAddress,
    end: VirtualAddress,
}

impl VirtualRange {
    pub const fn new(start: VirtualAddress, size: usize) -> Self {
        Self { start, size }
    }

    pub fn end(&self) -> VirtualAddress {
        self.start + self.size
    }

    pub const fn size(&self) -> usize {
        self.size
    }
}

impl IntoIterator for &VirtualRange {
    type Item = VirtualAddress;
    type IntoIter = VirtualRangeIter;

    fn into_iter(self) -> VirtualRangeIter {
        VirtualRangeIter {
            current: self.start,
            end: self.end(),
        }
    }
}

impl Iterator for VirtualRangeIter {
    type Item = VirtualAddress;

    fn next(&mut self) -> Option<VirtualAddress> {
        if self.current < self.end {
            let ret = self.current;
            self.current += Page::SIZE;
            Some(ret)
        } else {
            None
        }
    }
}

enum VirtualBacking {
    Present(Page),
    Empty,
}

/// A virtual memory region.
pub struct VirtualRegion {
    range: VirtualRange,
    backing: Vec<VirtualBacking>,
}
