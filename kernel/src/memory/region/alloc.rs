use alloc::sync::Arc;
use alloc::vec::Vec;

use spin::Mutex;

use super::VirtualRegion;
use crate::memory::Frame;

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
        if size % Frame::SIZE != 0 {
            None
        } else {
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
    }

    pub fn free(&self, _region: &mut VirtualRegion) {
        todo!()
    }
}
