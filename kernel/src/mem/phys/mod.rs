mod alloc;
mod arch;

pub use self::alloc::PageAllocator;
pub use arch::Page;

use super::map::MmapEntry;

#[repr(u16)]
pub enum PageType {
    Unallocated = 0,
    Allocated = 1 << 9,
}

pub fn init(mmap: &[MmapEntry]) {
    alloc::init(mmap);
}
