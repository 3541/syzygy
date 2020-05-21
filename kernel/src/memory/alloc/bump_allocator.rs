use alloc::alloc::{GlobalAlloc, Layout};
use core::ptr;

use logc::{error, trace};

use crate::memory::{Address, VirtualAddress};
use crate::sync::SpinLocked;

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
            heap_end: VirtualAddress::new_const(heap_base.raw() + heap_size),
            next: heap_base,
            count: 0,
        }
    }

    pub unsafe fn alloc(&mut self, layout: Layout) -> *mut u8 {
        trace!("{:x?} allocating {:x?}", *self, layout);

        let ret = self.next.next_aligned_addr(layout.align());
        let end = match ret.checked_add(layout.size()) {
            Some(end) => VirtualAddress::new(end),
            None => return ptr::null_mut(),
        };

        if end > self.heap_end {
            error!("Out of kernel heap!");
            ptr::null_mut()
        } else {
            self.next = end;
            self.count += 1;
            trace!("Allocated {} to {} (0x{:x})", ret, end, end - ret);
            *ret as *mut u8
        }
    }

    pub unsafe fn dealloc(&mut self, _ptr: *mut u8, _layout: Layout) {
        trace!("{:x?} deallocating", *self);

        self.count -= 1;
        if self.count == 0 {
            self.next = self.heap_base;
        }
    }
}
