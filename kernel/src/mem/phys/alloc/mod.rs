//! Physical memory allocation.

mod bitmap;

use alloc::vec::Vec;

use log_crate::debug;

use super::Page;
use crate::mem::map::{MmapEntry, MmapEntryType};
use bitmap::BitmapAllocator;

/// A page allocator over a memory region.
pub trait RegionPageAllocator {
    /// Construct the allocator over the given region.
    fn new(area: &MmapEntry) -> Self;
    /// Check whether the given page falls inside the allocator's region.
    fn owns(&self, page: &Page) -> bool;
    /// Allocate a page.
    fn alloc(&self) -> Option<Page>;
    /// Free a page.
    fn dealloc(&self, page: Page);
}

/// A global page allocator composed of multiple region allocators.
pub struct PageAllocator<A: RegionPageAllocator>(Vec<A>);

singleton!(PAGE_ALLOCATOR, PageAllocator<BitmapAllocator>);

impl<A: RegionPageAllocator> PageAllocator<A> {
    /// Initialize the page allocator from the memory map. Creates a region
    /// allocator over each usable entry.
    pub fn new(mmap: &[MmapEntry]) -> Self {
        Self(
            mmap.iter()
                .filter(|entry| entry.entry_type == MmapEntryType::Usable)
                .inspect(|entry| debug!("INITIALIZING BitmapAllocator over region {}.", entry))
                .map(A::new)
                .collect(),
        )
    }

    /// Allocate a page from any of the regions.
    pub fn alloc(&self) -> Option<Page> {
        for area in &self.0 {
            if let s @ Some(_) = area.alloc() {
                return s;
            }
        }

        None
    }

    /// Free a page to the correct region.
    pub fn dealloc(&self, page: Page) {
        for area in &self.0 {
            if area.owns(&page) {
                area.dealloc(page);
                return;
            }
        }
    }
}

/// Initialize the page allocator.
pub fn init(mmap: &[MmapEntry]) {
    PAGE_ALLOCATOR.init(PageAllocator::new(mmap));
    debug!("INITIALIZED PageAllocator.");
}
