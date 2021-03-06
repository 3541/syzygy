//! x86_64 physical memory management.

use crate::mem::phys::PageType;
use crate::mem::{size, Address, PageAllocator, PhysicalAddress};

/// A physical page. Bit 9 signifies the page's type, either allocated or not.
/// Allocated pages came from a page allocator and are returned to it on
/// [drop](Page::drop).
pub struct Page(usize);

impl Page {
    /// The page size.
    pub const SIZE: usize = 4 * size::KB;

    /// Create a new page of the given address and type.
    /// # Safety
    /// If `ty` is [`Allocated`](PageType::Allocated), the page must genuinely
    /// have come from a page allocator.
    pub unsafe fn new(address: PhysicalAddress, ty: PageType) -> Page {
        assert!(address.is_aligned(Self::SIZE));
        Page(address.raw() | ty as usize)
    }

    /// Check whether the page was allocated or not.
    fn allocated(&self) -> bool {
        self.0 & PageType::Allocated as usize == PageType::Allocated as usize
    }

    /// Get the page's base address.
    pub fn address(&self) -> PhysicalAddress {
        // Get rid of the allocated flag.
        PhysicalAddress::new(self.0 & !(PageType::Allocated as usize))
    }
}

impl Drop for Page {
    /// Return the page to the allocator if necessary.
    fn drop(&mut self) {
        if !self.allocated() {
            return;
        }

        // A hack, because dealloc takes ownership while drop has a reference.
        PageAllocator::the().dealloc(Page(self.0));
    }
}
