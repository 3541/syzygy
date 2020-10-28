use crate::mem::phys::PageType;
use crate::mem::{size, Address, PageAllocator, PhysicalAddress};

// Bit 9 signifies the page's type. Allocated pages came from the page allocator
// and are freed on drop.
pub struct Page(usize);

impl Page {
    pub const SIZE: usize = 4 * size::KB;

    pub fn new(address: PhysicalAddress, ty: PageType) -> Page {
        Page(address.raw() | ty as usize)
    }

    fn allocated(&self) -> bool {
        self.0 & PageType::Allocated as usize == PageType::Allocated as usize
    }

    pub fn address(&self) -> PhysicalAddress {
        // Get rid of the allocated flag.
        PhysicalAddress::new(self.0 & !(PageType::Allocated as usize))
    }
}

impl Drop for Page {
    fn drop(&mut self) {
        if !self.allocated() {
            return;
        }

        // A hack, because dealloc takes ownership while drop has a reference.
        PageAllocator::the().dealloc(Page(self.0));
    }
}
