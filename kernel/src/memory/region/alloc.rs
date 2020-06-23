use alloc::vec;
use alloc::vec::Vec;

use super::VirtualRegion;

pub struct VirtualRegionAllocator<const UNIT: usize> {
    free: Vec<VirtualRegion>,
    range: VirtualRegion,
}

impl<const UNIT: usize> VirtualRegionAllocator<{ UNIT }> {
    pub fn new(range: VirtualRegion) -> Self {
        VirtualRegionAllocator {
            free: vec![range.clone()],
            range,
        }
    }

    pub fn alloc(&mut self, size: usize) -> Option<VirtualRegion> {
        if size % UNIT != 0 {
            None
        } else {
            let from_region_index = self
                .free
                .iter_mut()
                .enumerate()
                .filter(|(_, r)| r.size >= size)
                .nth(0)?
                .0;

            // Use a null Region as a placeholder.
            self.free.push(VirtualRegion::null());
            let from_region = self.free.swap_remove(from_region_index);
            match from_region.cleave(size) {
                Ok((region, leftover)) => {
                    self.free[from_region_index] = leftover;
                    Some(region)
                }
                Err(original) => {
                    self.free[from_region_index] = original;
                    None
                }
            }
        }
    }
}
