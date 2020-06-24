pub mod alloc;

use ::alloc::sync::Arc;

use super::paging::mapper::Mapper;
use super::paging::EntryFlags;
use super::phys::{Frame, PhysicalMemory, PHYSICAL_ALLOCATOR};
use super::{Address, VirtualAddress};

pub use self::alloc::VirtualRegionAllocator;

#[derive(Clone, Debug)]
pub struct VirtualRegion {
    start: VirtualAddress,
    size: usize,
    backing: Option<Arc<PhysicalMemory>>,
}

impl VirtualRegion {
    pub fn null() -> Self {
        VirtualRegion {
            start: VirtualAddress::new(0),
            size: 0,
            backing: None,
        }
    }

    pub const fn start(&self) -> VirtualAddress {
        self.start
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn end(&self) -> VirtualAddress {
        self.start + self.size
    }

    pub fn cleave(self, size: usize) -> Result<(VirtualRegion, VirtualRegion), VirtualRegion> {
        if self.backing.is_some() || size > self.size - 8 || self.size % 8 != 0 {
            Err(self)
        } else {
            Ok((
                VirtualRegion {
                    start: self.start,
                    size,
                    backing: None,
                },
                VirtualRegion {
                    start: self.start + size,
                    size: self.size - size,
                    backing: None,
                },
            ))
        }
    }

    pub const unsafe fn new(start: VirtualAddress, size: usize) -> Self {
        VirtualRegion {
            start,
            size,
            backing: None,
        }
    }

    pub fn map_to(&mut self, mapper: &mut Mapper, memory: PhysicalMemory, flags: EntryFlags) {
        let frames = memory.frames();
        assert!(self.size / Frame::SIZE == frames.len());

        for (address, frame) in (self.start..self.start + self.size)
            .step_by(Frame::SIZE)
            .zip(frames)
        {
            mapper.map_to(address, frame, flags).flush();
        }

        self.backing = Some(Arc::new(memory));
    }

    pub fn map(&mut self, mapper: &mut Mapper, flags: EntryFlags) {
        assert!(self.backing.is_none());

        let memory = PHYSICAL_ALLOCATOR
            .alloc_memory(self.size)
            .expect("Failed to allocate physical memory.");
        self.map_to(mapper, memory, flags);
    }

    /*


    pub fn coalesce(
        self,
        other: VirtualRegion,
    ) -> Result<VirtualRegion, (VirtualRegion, VirtualRegion)> {
        if self.backing.is_some() || other.backing.is_some() {
            Err((self, other))
        } else if other.start < self.start {
            other.coalesce(self).map_err(|(o, s)| (s, o))
        } else if self.end() != other.start {
            Err((self, other))
        } else {
            Ok(VirtualRegion {
                start: self.start,
                size: self.size + other.size,
                backing: None,
            })
        }
    }*/
}
