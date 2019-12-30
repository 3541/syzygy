use alloc::alloc::{GlobalAlloc, Layout};
use core::ptr::null_mut;

pub struct FakeAllocator;

unsafe impl GlobalAlloc for FakeAllocator {
    unsafe fn alloc(&self, _layout: Layout) -> *mut u8 {
        null_mut()
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        unimplemented!("Fake allocator.")
    }
}
