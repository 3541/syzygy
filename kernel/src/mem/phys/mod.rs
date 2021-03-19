//! Physical memory management.

mod alloc;
mod arch;

pub use self::alloc::PageAllocator;
pub use arch::{Page, PageRef};

use super::map::MmapEntry;

/// Whether a page came from a page allocator or not.
#[repr(u16)]
pub enum PageType {
    Unallocated = 0,
    Allocated = 1 << 9,
}

/// Initialize the page allocator.
pub fn init(mmap: &[MmapEntry]) {
    alloc::init(mmap);
}
