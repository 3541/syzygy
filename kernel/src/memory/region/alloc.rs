use alloc::sync::Arc;
use alloc::vec::Vec;
use core::mem::size_of;
use core::ptr;

use spin::Mutex;

use super::{TypedRegion, VirtualRegion};
use crate::memory::paging::EntryFlags;
use crate::memory::{align_up, Address, Frame, PhysicalAddress, PhysicalMemory};
use crate::task::Task;

#[derive(Debug)]
pub struct VirtualRegionAllocator {
    free: Mutex<Vec<VirtualRegion>>,
    _range: VirtualRegion,
}

impl Clone for VirtualRegionAllocator {
    fn clone(&self) -> VirtualRegionAllocator {
        VirtualRegionAllocator {
            free: Mutex::new(self.free.lock().clone()),
            _range: self._range.clone(),
        }
    }
}

impl VirtualRegionAllocator {
    pub fn new(mut range: VirtualRegion) -> Arc<VirtualRegionAllocator> {
        assert!(!range.is_mapped() && !range.was_allocated());
        let ret = Arc::new(VirtualRegionAllocator {
            free: Mutex::new(Vec::new()),
            _range: range.clone(),
        });

        range.allocator = Some(Arc::downgrade(&ret));
        ret.free.lock().push(range);

        ret
    }

    pub fn alloc(&self, size: usize) -> Option<VirtualRegion> {
        let size = align_up(size, Frame::SIZE);
        let mut free = self.free.lock();
        let from_region_index = free
            .iter_mut()
            .enumerate()
            .filter(|(_, r)| r.size >= size)
            .nth(0)?
            .0;

        // Use a null Region as a placeholder.
        free.push(VirtualRegion::null());
        let from_region = free.swap_remove(from_region_index);
        match from_region.cleave(size) {
            Ok((region, leftover)) => {
                free[from_region_index] = leftover;
                Some(region)
            }
            Err(original) => {
                free[from_region_index] = original;
                None
            }
        }
    }

    pub unsafe fn make_mapped<T>(
        &self,
        address: PhysicalAddress,
        flags: EntryFlags,
    ) -> Option<TypedRegion<T>> {
        let region_base = address.previous_aligned(Frame::SIZE);
        let offset = address - region_base;
        let mut region = self.alloc(size_of::<T>() + offset)?;
        region.map_to(
            &mut Task::current().lock().pager().active_table(),
            Arc::new(PhysicalMemory::region(region_base, size_of::<T>())),
            flags,
        );

        Some(region.into_typed(offset))
    }

    pub fn free(&self, region: &mut VirtualRegion) {
        assert!(ptr::eq(
            &*region.allocator.as_ref().unwrap().upgrade().unwrap(),
            self
        ));
        // Correctness of unmap is checked therein.
        region.unmap(&mut Task::current().lock().pager().active_table());

        self.free.lock().push(region.clone());
    }
}
