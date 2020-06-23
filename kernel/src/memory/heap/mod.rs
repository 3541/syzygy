mod bump_allocator;
//mod fake_allocator;

use alloc::alloc::{GlobalAlloc, Layout};

use logc::trace;

use super::paging::mapper::Mapper;
use super::paging::table::EntryFlags;
use super::size::{KB, MB};
use super::{Address, Frame, VirtualAddress, VirtualRegion};
use crate::sync::SpinLocked;
use crate::task;
use bump_allocator::BumpAllocator;
//use fake_allocator::FakeAllocator;

pub const HEAP_BASE: VirtualAddress = unsafe { VirtualAddress::new_const(0x0000_4EA6_0000_0000) };
pub const HEAP_SIZE: usize = 4 * MB;

static mut INIT_HEAP: [u8; 200 * KB] = [0; 200 * KB];

#[cfg(not(test))]
#[global_allocator]
static ALLOCATOR: SpinLocked<GlobalAllocator> =
    unsafe { SpinLocked::new(GlobalAllocator::new(HEAP_BASE, HEAP_SIZE)) };

struct GlobalAllocator {
    heap: Option<BumpAllocator>,
    init_heap: Option<BumpAllocator>,
}

impl GlobalAllocator {
    pub const unsafe fn new(heap_base: VirtualAddress, heap_size: usize) -> Self {
        Self {
            heap: None,
            init_heap: None,
        }
    }

    pub unsafe fn init(&mut self) {
        self.init_heap = Some(BumpAllocator::new(
            VirtualAddress::new(INIT_HEAP.as_ptr() as usize),
            INIT_HEAP.len(),
        ));
    }

    pub unsafe fn add_heap(&mut self, region: VirtualRegion) {
        self.heap = Some(BumpAllocator::new(region.start(), region.size()))
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
        let heap = this.heap.as_mut().unwrap();

        if *heap.base() <= (ptr as usize) && (ptr as usize) < *heap.end() {
            heap.dealloc(ptr, layout);
        }
    }
}

pub unsafe fn init_allocator() {
    ALLOCATOR.lock().init();
}

pub fn init_heap() {
    trace!(
        "Mapping heap from {} to {} (0x{:x})",
        HEAP_BASE,
        HEAP_BASE + HEAP_SIZE,
        HEAP_SIZE
    );
    let task_list = task::task_list();
    let task = task_list.current();

    let mut pager = task.pager();
    let mut heap = pager
        .allocator()
        .alloc(HEAP_SIZE)
        .expect("Unable to allocate memory for the main heap.");

    heap.map(pager.mapper(), EntryFlags::PRESENT | EntryFlags::WRITABLE);

    // Notify the bump allocator that the real heap is now available.
    unsafe { ALLOCATOR.lock().add_heap(heap) };
}
