//! x86_64 physical memory management.

use core::fmt;
use core::mem::forget;

use crate::mem::phys::PageType;
use crate::mem::{size, Address, PageAllocator, PhysicalAddress};

/// A physical page. Bit 9 signifies the page's type, either allocated or not. Allocated pages came
/// from a page allocator and are returned to it on [drop](Page::drop). Pages should be
/// single-owner, and sharing accomplished on a higher level.
pub struct Page(usize);

impl Page {
    /// The page size.
    pub const SIZE: usize = 4 * size::KB;

    /// Create a new page of the given address and type. # Safety If `ty` is
    /// [`Allocated`](PageType::Allocated), the page must genuinely have come from a page allocator.
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

    /// Leak the page, returning its address.
    pub fn leak(self) -> PhysicalAddress {
        let addr = self.address();
        forget(self);
        addr
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

impl fmt::Debug for Page {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Page({})", self.address())
    }
}

/// A non-owning reference to a page.
pub struct PageRef(pub PhysicalAddress);

impl PageRef {
    pub const fn new(addr: PhysicalAddress) -> Self {
        Self(addr)
    }

    pub fn address(&self) -> PhysicalAddress {
        self.0
    }

    /// SAFETY: This should only be used to acquire owning Pages referring to genuinely un-owned
    /// memory.
    unsafe fn to_owned(self, ty: PageType) -> Page {
        Page::new(self.0, ty)
    }
}
