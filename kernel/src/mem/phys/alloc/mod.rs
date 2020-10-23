mod bitmap;

use alloc::vec::Vec;

use log_crate::debug;

use super::Page;
use crate::mem::map::{Mmap, MmapEntry, MmapEntryType};
use crate::util::sync::OnceCell;
use bitmap::BitmapAllocator;

pub trait RegionPageAllocator {
    fn new(area: &MmapEntry) -> Self;
    fn owns(&self, page: &Page) -> bool;
    fn alloc(&self) -> Option<Page>;
    fn dealloc(&self, page: Page);
}

// A RegionPageAllocator only owns a specific interval of memory. Since physical
// memory is composed of disjoint regions, the global physical allocator must
// deal with multiple region allocators.
pub struct PageAllocator<A: RegionPageAllocator>(Vec<A>);

static PAGE_ALLOCATOR: OnceCell<PageAllocator<BitmapAllocator>> = OnceCell::new();

impl PageAllocator<BitmapAllocator> {
    pub fn the() -> &'static Self {
        &*PAGE_ALLOCATOR
    }
}

impl<A: RegionPageAllocator> PageAllocator<A> {
    pub fn new(mmap: &Mmap) -> Self {
        Self(
            mmap.iter()
                .filter(|entry| entry.entry_type == MmapEntryType::Usable)
                .inspect(|entry| debug!("INITIALIZING BitmapAllocator over region {}.", entry))
                .map(A::new)
                .collect(),
        )
    }

    pub fn alloc(&self) -> Option<Page> {
        for area in &self.0 {
            if let s @ Some(_) = area.alloc() {
                return s;
            }
        }

        None
    }

    pub fn dealloc(&self, page: Page) {
        for area in &self.0 {
            if area.owns(&page) {
                area.dealloc(page);
                return;
            }
        }
    }
}

pub fn init(mmap: &Mmap) {
    PAGE_ALLOCATOR.init(PageAllocator::new(mmap));
    debug!("INITIALIZED PageAllocator.");
}
