pub mod alloc;

use ::alloc::vec::Vec;
use core::fmt;
use core::ops::Drop;

use super::paging::EntryFlags;
use super::{Address, PhysicalAddress, VirtualRegion};
use crate::task::current_task;

pub use self::alloc::PHYSICAL_ALLOCATOR;

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct Frame(pub PhysicalAddress);

impl Frame {
    pub const SIZE: usize = 4096;

    pub fn address(&self) -> PhysicalAddress {
        self.0
    }

    /*    pub fn end_address(&self) -> PhysicalAddress {
        self.0 + Self::SIZE
    }*/

    /*    pub fn containing_address(address: PhysicalAddress) -> Frame {
        Frame(address.previous_aligned(Self::SIZE))
    }*/

    /*    pub fn range_inclusive(from: Frame, to: Frame) -> FrameIterator {
        FrameIterator { from, to }
    }*/
}

impl Drop for Frame {
    fn drop(&mut self) {
        unsafe { PHYSICAL_ALLOCATOR.free(self) };
    }
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Frame({})", self.0)
    }
}

#[derive(Debug)]
pub enum PhysicalMemoryKind {
    Allocated,
    Region,
}

#[derive(Debug)]
pub struct PhysicalMemory {
    frames: Vec<Frame>,
    kind: PhysicalMemoryKind,
}

impl PhysicalMemory {
    pub unsafe fn region(start: PhysicalAddress, size: usize) -> PhysicalMemory {
        let frames = (*start.previous_aligned(Frame::SIZE)
            ..=*(start + size - 1).previous_aligned(Frame::SIZE))
            .step_by(Frame::SIZE)
            .map(|a| Frame(PhysicalAddress::new_unchecked(a)))
            .collect();
        PhysicalMemory {
            frames,
            kind: PhysicalMemoryKind::Region,
        }
    }

    pub fn size(&self) -> usize {
        self.frames.len() * Frame::SIZE
    }

    pub fn frames(&self) -> &[Frame] {
        &self.frames
    }

    pub fn into_frames(self) -> Vec<Frame> {
        self.frames
    }

    pub fn map_for_kernel(self, flags: EntryFlags) -> Option<VirtualRegion> {
        let task = current_task();
        let mut pager = task.pager();
        let allocator = pager.kernel_allocator();

        let mut mapping = allocator.alloc(self.size())?;

        mapping.map_to(pager.mapper(), self, flags);
        Some(mapping)
    }
}
