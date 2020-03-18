use alloc::alloc::{GlobalAlloc, Layout};
use core::ptr;

use spin::Mutex;

use super::MutexWrapper;
use crate::memory::{next_aligned_addr, VirtualAddress};

#[derive(Debug)]
pub struct BumpAllocator {
    heap_base: VirtualAddress,
    heap_end: VirtualAddress,
    next: VirtualAddress,
    count: usize,
}

impl BumpAllocator {
    pub const unsafe fn new(heap_base: VirtualAddress, heap_size: usize) -> Self {
        Self {
            heap_base,
            heap_end: heap_base + heap_size,
            next: heap_base,
            count: 0,
        }
    }
}

unsafe impl GlobalAlloc for MutexWrapper<BumpAllocator> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let mut this = self.lock();

        trace!("{:#x?} allocating {:x?}", *this, layout);

        let ret = next_aligned_addr(this.next, layout.align());
        let end = match ret.checked_add(layout.size()) {
            Some(end) => end,
            None => return ptr::null_mut(),
        };

        if end > this.heap_end {
            ptr::null_mut()
        } else {
            this.next = end;
            this.count += 1;
            debug!("Allocated 0x{:x} to 0x{:x} (0x{:x})", ret, end, end - ret);
            ret as *mut u8
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        let mut this = self.lock();

        this.count -= 1;
        if this.count == 0 {
            this.next = this.heap_base;
        }
    }
}
