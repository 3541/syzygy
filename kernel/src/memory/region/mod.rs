pub mod alloc;

use ::alloc::sync::{Arc, Weak};
use core::mem::{forget, replace};
use core::ops::{Deref, DerefMut};
use core::ptr::Unique;

use super::paging::mapper::Mapper;
use super::paging::EntryFlags;
use super::phys::{Frame, PhysicalMemory, PhysicalMemoryAllocator};
use super::{align_up, Address, VirtualAddress};

pub use self::alloc::VirtualRegionAllocator;

#[derive(Clone, Debug)]
pub struct VirtualRegion {
    start: VirtualAddress,
    size: usize,
    backing: Option<Arc<PhysicalMemory>>,
    allocator: Option<Weak<VirtualRegionAllocator>>,
}

impl VirtualRegion {
    pub fn null() -> Self {
        VirtualRegion {
            start: VirtualAddress::new(0),
            size: 0,
            backing: None,
            allocator: None,
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

    pub fn contains(&self, address: VirtualAddress) -> bool {
        self.start <= address && address < self.end()
    }

    /*    pub fn backing(&self) -> Option<&Arc<PhysicalMemory>> {
        self.backing.as_ref()
    }*/

    pub fn is_mapped(&self) -> bool {
        self.backing.is_some()
    }

    pub fn was_allocated(&self) -> bool {
        self.allocator.is_some()
    }

    pub fn pages(&self) -> impl Iterator<Item = VirtualAddress> {
        (self.start..(self.start + self.size)).step_by(Frame::SIZE)
    }

    pub fn cleave(self, size: usize) -> Result<(VirtualRegion, VirtualRegion), VirtualRegion> {
        if self.backing.is_some() || size > self.size - 8 || self.size % 8 != 0 {
            Err(self)
        } else {
            let ret = Ok((
                VirtualRegion {
                    start: self.start,
                    size,
                    backing: None,
                    allocator: self.allocator.clone(),
                },
                VirtualRegion {
                    start: self.start + size,
                    size: self.size - size,
                    backing: None,
                    allocator: self.allocator.clone(),
                },
            ));
            forget(self);
            ret
        }
    }

    pub const unsafe fn new(start: VirtualAddress, size: usize) -> VirtualRegion {
        VirtualRegion {
            start,
            size,
            backing: None,
            allocator: None,
        }
    }

    pub fn map_to(&mut self, mapper: &mut Mapper, memory: Arc<PhysicalMemory>, flags: EntryFlags) {
        let frames = memory.frames();
        assert!(self.size / Frame::SIZE == frames.len());

        for (address, frame) in (self.start..self.start + self.size)
            .step_by(Frame::SIZE)
            .zip(frames)
        {
            mapper.map_to(address, frame, flags).flush();
        }

        self.backing = Some(memory);
    }

    pub fn map(&mut self, mapper: &mut Mapper, flags: EntryFlags) {
        assert!(self.backing.is_none());

        let memory = PhysicalMemoryAllocator::the()
            .alloc_memory(self.size)
            .expect("Failed to allocate physical memory.");
        self.map_to(mapper, Arc::new(memory), flags);
    }

    pub fn into_typed<T>(self, offset: usize) -> TypedRegion<T> {
        TypedRegion {
            inner: Unique::new((self.start() + offset).as_mut_ptr()).unwrap(),
            region: self,
        }
    }

    /// # Safety
    /// See the comment on `as_mut_fat_ptr`.
    pub unsafe fn into_typed_unsized<T: ?Sized>(
        self,
        offset: usize,
        param: usize,
    ) -> TypedRegion<T> {
        TypedRegion {
            inner: Unique::new((self.start() + offset).as_mut_fat_ptr(param)).unwrap(),
            region: self,
        }
    }

    pub fn grow(self, new_size: usize) -> VirtualRegion {
        let new_size = align_up(new_size, Frame::SIZE);
        if new_size <= self.size {
            return self;
        }

        todo!(
            "Actual region growing. Old size: {}, new size: {}",
            self.size,
            new_size
        )
    }

    pub fn unmap(&mut self, mapper: &mut Mapper) {
        if let Some(ref memory) = self.backing {
            if Arc::strong_count(memory) == 1 {
                self.pages().for_each(|page| mapper.unmap(page).flush());
            }
        }

        self.backing = None;
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

impl Drop for VirtualRegion {
    fn drop(&mut self) {
        if self.size == 0 {
            return;
        }

        if let Some(allocator) = self.allocator.as_mut().map(|a| a.upgrade()).flatten() {
            allocator.free(self);
        }
    }
}

pub struct TypedRegion<T: ?Sized> {
    region: VirtualRegion,
    inner: Unique<T>,
}

impl<T: ?Sized> TypedRegion<T> {
    pub fn offset(&self) -> usize {
        // Need to cast through a thin pointer to discard the potential fat pointer.
        self.inner.as_ptr() as *const () as usize - *self.region.start()
    }

    pub fn into_region(mut self) -> VirtualRegion {
        replace(&mut self.region, VirtualRegion::null())
    }
}

impl<T: ?Sized> Deref for TypedRegion<T> {
    type Target = T;

    fn deref(&self) -> &T {
        assert!(self.region.is_mapped());

        unsafe { self.inner.as_ref() }
    }
}

impl<T: ?Sized> DerefMut for TypedRegion<T> {
    fn deref_mut(&mut self) -> &mut T {
        assert!(self.region.is_mapped());

        unsafe { self.inner.as_mut() }
    }
}

impl<T: ?Sized> Drop for TypedRegion<T> {
    fn drop(&mut self) {
        drop(unsafe { self.inner.as_mut() });
    }
}
