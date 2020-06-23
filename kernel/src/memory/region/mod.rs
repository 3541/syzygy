//pub mod alloc;

use ::alloc::sync::Arc;

use super::paging::mapper::Mapper;
use super::paging::EntryFlags;
use super::phys::{PhysicalMemory, PHYSICAL_ALLOCATOR};
use super::{Address, VirtualAddress};

//pub use self::alloc::VirtualRegionAllocator;

#[derive(Clone)]
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

    pub fn start(&self) -> VirtualAddress {
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
                    start: self.end(),
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

    pub fn map(&mut self, mapper: &mut Mapper, flags: EntryFlags) {
        assert!(self.backing.is_none());

        let mem = PHYSICAL_ALLOCATOR
            .alloc_memory(self.size)
            .expect("Failed to allocate physical memory.");
        self.backing = Some(Arc::new(mem));
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
