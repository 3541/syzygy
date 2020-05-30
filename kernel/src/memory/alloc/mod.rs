mod bump_allocator;
//mod fake_allocator;

use alloc::alloc::{GlobalAlloc, Layout};

use super::heap::{HEAP_BASE, HEAP_SIZE};
use crate::memory::size::KB;
use crate::memory::{Address, VirtualAddress};
use crate::sync::SpinLocked;
use bump_allocator::BumpAllocator;
//use fake_allocator::FakeAllocator;

//#[global_allocator]
//static ALLOCATOR: FakeAllocator = FakeAllocator {};

static mut INIT_HEAP: [u8; 200 * KB] = [0; 200 * KB];

#[cfg(not(test))]
#[global_allocator]
/*static ALLOCATOR: SpinLocked<BumpAllocator> =
unsafe { SpinLocked::new(BumpAllocator::new(HEAP_BASE, HEAP_SIZE)) };*/
static ALLOCATOR: SpinLocked<GlobalAllocator> =
    unsafe { SpinLocked::new(GlobalAllocator::new(HEAP_BASE, HEAP_SIZE)) };

struct GlobalAllocator {
    heap: Option<BumpAllocator>,
    heap_base: VirtualAddress,
    heap_size: usize,
    init_heap: Option<BumpAllocator>,
}

impl GlobalAllocator {
    pub const unsafe fn new(heap_base: VirtualAddress, heap_size: usize) -> Self {
        Self {
            heap: None,
            heap_base,
            heap_size,
            init_heap: None,
        }
    }

    pub unsafe fn init(&mut self) {
        self.init_heap = Some(BumpAllocator::new(
            VirtualAddress::new(INIT_HEAP.as_ptr() as usize),
            INIT_HEAP.len(),
        ));
    }

    pub unsafe fn add_heap(&mut self) {
        self.heap = Some(BumpAllocator::new(self.heap_base, self.heap_size))
    }
}

unsafe impl GlobalAlloc for SpinLocked<GlobalAllocator> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let mut this = self.lock();

        match &mut this.heap {
            Some(h) => h.alloc(layout),
            None => this.init_heap.as_mut().unwrap().alloc(layout),
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let mut this = self.lock();

        if *this.heap_base <= (ptr as usize) && (ptr as usize) < *this.heap_base + this.heap_size {
            this.heap.as_mut().unwrap().dealloc(ptr, layout);
        }
    }
}

pub unsafe fn init_allocator() {
    ALLOCATOR.lock().init();
}

pub unsafe fn add_heap() {
    ALLOCATOR.lock().add_heap();
}
