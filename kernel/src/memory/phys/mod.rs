pub mod alloc;

use ::alloc::sync::Arc;
use ::alloc::vec::Vec;
use core::fmt;
use core::mem::forget;
use core::ops::Drop;

use super::paging::EntryFlags;
use super::{Address, PhysicalAddress, VirtualRegion};
use crate::task::Task;

pub use self::alloc::PhysicalMemoryAllocator;

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct Frame(pub PhysicalAddress);

impl Frame {
    pub const SIZE: usize = 4096;

    pub fn address(&self) -> PhysicalAddress {
        self.0
    }
}

impl Drop for Frame {
    fn drop(&mut self) {
        unsafe { PhysicalMemoryAllocator::the().free(self) };
    }
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Frame({})", self.0)
    }
}

#[derive(Debug, PartialEq)]
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
        let frames: Vec<_> = (*start.previous_aligned(Frame::SIZE)
            ..=*(start + size - 1).previous_aligned(Frame::SIZE))
            .step_by(Frame::SIZE)
            .map(|a| Frame(PhysicalAddress::new_unchecked(a)))
            .collect();
        assert!(frames.len() > 0);
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

    /*
    This will be needed to implement virtual region growing.
    pub unsafe fn grow_region(&mut self, new_size: usize) {
        assert!(self.kind == PhysicalMemoryKind::Region);
        let new_size = align_up(new_size, Frame::SIZE);

        if new_size <= self.size() {
            return;
        }

        let difference = new_size - self.size();

        let start = self.frames.last().expect("Memory empty.").end();
        (start..(start + difference))
            .step_by(Frame::SIZE)
            .for_each(|a| self.frames.push(Frame(a)));
    }*/

    pub fn map_for_kernel(self, flags: EntryFlags) -> Option<VirtualRegion> {
        let task = Task::current();
        let lock = task.lock();
        let pager = lock.pager();
        let allocator = pager.kernel_allocator();

        let mut mapping = allocator.alloc(self.size())?;

        mapping.map_to(&mut pager.active_table(), Arc::new(self), flags);
        Some(mapping)
    }
}

impl Drop for PhysicalMemory {
    fn drop(&mut self) {
        match self.kind {
            PhysicalMemoryKind::Region => self.frames.drain(0..).for_each(forget),
            _ => {}
        }
    }
}
