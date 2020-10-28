mod bump_allocator;
//mod fake_allocator;

use alloc::alloc::{GlobalAlloc, Layout};

use logc::debug;

use super::paging::table::EntryFlags;
use super::size::{KB, MB};
use super::{Address, VirtualAddress, VirtualRegion};
use crate::sync::SpinLock;
use crate::task::Scheduler;
use bump_allocator::BumpAllocator;
//use fake_allocator::FakeAllocator;

pub const HEAP_SIZE: usize = 4 * MB;

static mut INIT_HEAP: [u8; 400 * KB] = [0; 400 * KB];

#[cfg(not(test))]
#[global_allocator]
static ALLOCATOR: SpinLock<GlobalAllocator> = unsafe { SpinLock::new(GlobalAllocator::new()) };

struct GlobalAllocator {
    heap: Option<BumpAllocator>,
    init_heap: Option<BumpAllocator>,
}

impl GlobalAllocator {
    pub const unsafe fn new() -> Self {
        Self {
            heap: None,
            init_heap: None,
        }
    }

    pub unsafe fn init(&mut self) {
        self.init_heap = Some(BumpAllocator::new(VirtualRegion::new(
            VirtualAddress::new(INIT_HEAP.as_ptr() as usize),
            INIT_HEAP.len(),
        )));
    }

    pub unsafe fn add_heap(&mut self, region: VirtualRegion) {
        self.heap = Some(BumpAllocator::new(region))
    }
}

unsafe impl GlobalAlloc for SpinLock<GlobalAllocator> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let mut this = self.lock();

        match &mut this.heap {
            Some(h) => h.alloc(layout),
            None => this.init_heap.as_mut().unwrap().alloc(layout),
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let mut this = self.lock();

        if let Some(h) = this.heap.as_mut() {
            if *h.base() <= (ptr as usize) && (ptr as usize) < *h.end() {
                h.dealloc(ptr, layout);
            } else {
                this.init_heap.as_mut().unwrap().dealloc(ptr, layout);
            }
        } else {
            this.init_heap.as_mut().unwrap().dealloc(ptr, layout);
        }
    }
}

#[cfg(not(test))]
pub unsafe fn init_allocator() {
    ALLOCATOR.lock().init();
}

#[cfg(not(test))]
pub fn init_heap() {
    let task_list = Scheduler::the();
    let task = task_list.current().lock();

    let pager = task.pager();
    let mut heap = pager
        .kernel_allocator()
        .alloc(HEAP_SIZE)
        .expect("Unable to allocate memory for the main heap.");

    heap.map(
        &mut pager.active_table(),
        EntryFlags::PRESENT | EntryFlags::WRITABLE,
    );

    debug!(
        "Mapped heap at {} - {} (0x{:x})",
        heap.start(),
        heap.end(),
        heap.size()
    );

    // Notify the bump allocator that the real heap is now available.
    unsafe { ALLOCATOR.lock().add_heap(heap) };
}
