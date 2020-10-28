use alloc::alloc::Layout;
use core::ptr;

use logc::{error, trace};

use crate::memory::{Address, VirtualAddress, VirtualRegion};

#[derive(Debug)]
pub struct BumpAllocator {
    backing: VirtualRegion,
    next: VirtualAddress,
    count: usize,
}

impl BumpAllocator {
    pub const unsafe fn new(backing: VirtualRegion) -> Self {
        Self {
            next: backing.start(),
            backing,
            count: 0,
        }
    }

    pub fn base(&self) -> VirtualAddress {
        self.backing.start()
    }

    pub fn end(&self) -> VirtualAddress {
        self.backing.end()
    }

    pub unsafe fn alloc(&mut self, layout: Layout) -> *mut u8 {
        trace!("{:x?} allocating {:x?}", *self, layout);

        let ret = self.next.next_aligned(layout.align());
        let end = match ret.checked_add(layout.size()) {
            Some(end) => VirtualAddress::new(end),
            None => return ptr::null_mut(),
        };

        if end > self.end() {
            error!("Out of kernel heap!");
            ptr::null_mut()
        } else {
            self.next = end;
            self.count += 1;
            trace!("Allocated {} to {} (0x{:x})", ret, end, end - ret);
            *ret as *mut u8
        }
    }

    pub unsafe fn dealloc(&mut self, ptr: *mut u8, _layout: Layout) {
        trace!("{:x?} deallocating", *self);

        if !self
            .backing
            .contains(VirtualAddress::new_unchecked(ptr as usize))
        {
            panic!("Tried to free un-allocated memory.");
        }

        self.count -= 1;
        if self.count == 0 {
            self.next = self.base();
        }
    }
}
