use core::mem::{align_of, size_of, transmute};

use super::VirtualAddress;
use crate::mem::size;

#[derive(Copy, Clone)]
pub struct VirtualRegion {
    start: VirtualAddress,
    size: usize,
}

impl VirtualRegion {
    pub const fn new(start: VirtualAddress, size: usize) -> VirtualRegion {
        VirtualRegion { start, size }
    }

    pub const unsafe fn from_slice<T>(slice: &mut [T]) -> VirtualRegion {
        VirtualRegion {
            start: VirtualAddress::from_ptr_unchecked(slice.as_ptr()),
            size: slice.len() * size_of::<T>(),
        }
    }

    pub const fn start(&self) -> VirtualAddress {
        self.start
    }

    pub const fn size(&self) -> usize {
        self.size
    }

    pub const unsafe fn as_mut_slice<T>(&mut self) -> &mut [T] {
        let start = self.start.next_aligned(align_of::<T>());
        let count = size::units_of::<T>(self.size - (start - self.start));
        transmute((start, count))
    }
}
