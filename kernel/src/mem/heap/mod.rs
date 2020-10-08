mod ll;
#[cfg(test)]
mod test;

use alloc::alloc::{GlobalAlloc, Layout};

use super::size;
use crate::util::sync::Transform;
use ll::{LLAlloc, LLNode};

// Statically allocated initial heap to allow for dynamic allocation before the
// actual VM system is up. *const () is used here to enforce pointer-size
// alignment.
const INIT_HEAP_SIZE: usize = 32 * size::KB;
static mut INIT_HEAP: [LLNode; size::units_of::<LLNode>(INIT_HEAP_SIZE)] =
    [LLNode::new(INIT_HEAP_SIZE); size::units_of::<LLNode>(INIT_HEAP_SIZE)];
// The initial heap uses LLAlloc because it is small, making space
// efficiency and defragmentation a higher priority than performance.
static mut INIT_ALLOCATOR: LLAlloc = unsafe { LLAlloc::from_slice(&mut INIT_HEAP) };

#[cfg_attr(not(test), global_allocator)]
static ALLOCATOR: Transform<&'static LLAlloc, &'static LLAlloc> =
    unsafe { Transform::new(&INIT_ALLOCATOR) };

unsafe impl<T: GlobalAlloc> GlobalAlloc for Transform<T, T> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.whichever().alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.whichever().dealloc(ptr, layout)
    }
}

#[cfg(not(test))]
#[alloc_error_handler]
fn alloc_err(layout: Layout) -> ! {
    panic!("Heap allocation error.\n  {:?}", layout);
}
