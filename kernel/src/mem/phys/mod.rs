mod alloc;
mod arch;

pub use self::alloc::PageAllocator;
pub use arch::Page;

use super::map::Mmap;

#[repr(u8)]
pub enum PageType {
    Unallocated = 0,
    Allocated = 1,
}

pub fn init(mmap: &Mmap) {
    alloc::init(mmap);
}
