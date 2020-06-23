pub mod alloc;

use ::alloc::vec::Vec;
use core::fmt;
use core::ops::Drop;

use crate::memory::{Address, PhysicalAddress};

pub use self::alloc::PHYSICAL_ALLOCATOR;

#[derive(Debug, Clone, PartialOrd, PartialEq)]
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
        Frame(address.previous_aligned(Self::SIZE))
    }

    /*    pub fn range_inclusive(from: Frame, to: Frame) -> FrameIterator {
        FrameIterator { from, to }
    }*/
}

impl Drop for Frame {
    fn drop(&mut self) {
        todo!()
    }
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Frame({})", self.0)
    }
}

/*pub struct FrameIterator {
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
}*/

pub enum PhysicalMemoryKind {
    Allocated,
    Region,
}

pub struct PhysicalMemory {
    frames: Vec<Frame>,
    kind: PhysicalMemoryKind,
}

impl PhysicalMemory {
    /*    pub fn coalesce(self, other: PhysicalMemory) -> PhysicalMemory {
            todo!()
    }*/
    pub unsafe fn region(start: PhysicalAddress, end: PhysicalAddress) -> PhysicalMemory {
        let frames = (*start..=*end.next_aligned(Frame::SIZE))
            .step_by(Frame::SIZE)
            .map(|a| Frame(PhysicalAddress::new_unchecked(a)))
            .collect();
        PhysicalMemory {
            frames,
            kind: PhysicalMemoryKind::Region,
        }
    }

    pub fn into_frames(self) -> Vec<Frame> {
        self.frames
    }
}
