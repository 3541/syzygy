//! A virtual memory region.

use alloc::vec::Vec;
use core::fmt;
use core::iter::IntoIterator;

use super::{
    ActivePrimaryTable, Flush, FlushAll, MappingFlags, Page, TActivePrimaryTable, VirtualAddress,
};
use crate::mem::PageType;

/// A virtual address range.
#[derive(Copy, Clone)]
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

    pub(super) unsafe fn to_owned(
        self,
        t: &ActivePrimaryTable,
        flags: MappingFlags,
        ty: PageType,
    ) -> Option<VirtualRegion> {
        Some(VirtualRegion {
            range: self,
            flags,
            backing: t
                .translate_range(self)?
                .into_iter()
                .map(|r| unsafe { r.map(|pr| pr.to_owned(ty)).into() })
                .collect(),
        })
    }
}

impl fmt::Display for VirtualRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} - {}]", self.start, self.end())
    }
}

impl fmt::Debug for VirtualRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
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

#[derive(Debug)]
pub(super) enum VirtualBacking {
    Present(Page),
    Empty,
}

impl VirtualBacking {
    fn as_ref(&self) -> Option<&Page> {
        match self {
            Self::Present(ref p) => Some(p),
            Self::Empty => None,
        }
    }
}

impl From<Option<Page>> for VirtualBacking {
    fn from(p: Option<Page>) -> Self {
        match p {
            Some(p) => Self::Present(p),
            None => Self::Empty,
        }
    }
}

/// A virtual memory region.
#[derive(Debug)]
pub struct VirtualRegion {
    range: VirtualRange,
    flags: MappingFlags,
    backing: Vec<VirtualBacking>,
}

impl VirtualRegion {
    pub const fn range(&self) -> VirtualRange {
        self.range
    }

    pub(super) unsafe fn take(self) -> (VirtualRange, Vec<VirtualBacking>) {
        (self.range, self.backing)
    }

    pub(super) unsafe fn create(
        range: VirtualRange,
        backing: Vec<VirtualBacking>,
        flags: MappingFlags,
    ) -> Self {
        Self {
            range,
            flags,
            backing,
        }
    }

    pub(super) fn new(
        t: &ActivePrimaryTable,
        from: VirtualRange,
        to: Vec<VirtualBacking>,
        flags: MappingFlags,
    ) -> Self {
        let to_ref: Vec<_> = to.iter().map(VirtualBacking::as_ref).collect();
        let mut flush = FlushAll::new();
        flush.consume_all(t.map_range(from, to_ref, flags));

        Self {
            range: from,
            flags,
            backing: to,
        }
    }

    pub fn map(&self, t: &ActivePrimaryTable) -> Vec<Flush> {
        t.map_range(
            self.range,
            self.backing.iter().map(VirtualBacking::as_ref).collect(),
            self.flags,
        )
    }
}
