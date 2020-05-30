pub mod alloc;

use core::fmt;

use crate::memory::{Address, PhysicalAddress};

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub struct Frame(pub PhysicalAddress);

impl Frame {
    pub const SIZE: usize = 4096;

    pub fn address(&self) -> PhysicalAddress {
        self.0
    }

    pub fn end_address(&self) -> PhysicalAddress {
        self.0 + Self::SIZE
    }

    pub fn containing_address(address: PhysicalAddress) -> Frame {
        Frame(address.prev_aligned_addr(Self::SIZE))
    }

    pub fn range_inclusive(from: Frame, to: Frame) -> FrameIterator {
        FrameIterator { from, to }
    }
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Frame({})", self.0)
    }
}

pub struct FrameIterator {
    from: Frame,
    to: Frame,
}

impl Iterator for FrameIterator {
    type Item = Frame;

    fn next(&mut self) -> Option<Frame> {
        if self.from.address() <= self.to.address() {
            let frame = self.from;
            self.from.0 += Frame::SIZE;
            Some(frame)
        } else {
            None
        }
    }
}

pub struct PhysicalMemory {}

impl PhysicalMemory {
    pub fn coalesce(self, other: PhysicalMemory) -> PhysicalMemory {
        todo!()
    }
}
