use alloc::vec;
use alloc::vec::Vec;
use core::mem::{forget, size_of};
use core::sync::atomic::{AtomicUsize, Ordering};

use super::RegionPageAllocator;
use crate::mem::map::MmapEntry;
use crate::mem::phys::{Page, PageType};
use crate::mem::{Address, PhysicalAddress};
use crate::util::sync::Spinlock;

pub struct BitmapAllocator {
    bitmap: Spinlock<Vec<usize>>,
    search_index: AtomicUsize,
    base: PhysicalAddress,
    size: usize,
}

impl BitmapAllocator {
    // The number of address space bytes covered by a single usize field of the
    // bitmap.
    const BYTES_PER_FIELD: usize = size_of::<usize>() * 8 * Page::SIZE;

    fn address(&self, index: usize, bit_index: usize) -> PhysicalAddress {
        self.base + (index * Self::BYTES_PER_FIELD + bit_index * Page::SIZE)
    }

    fn index_of(&self, page: &Page) -> usize {
        (page.address() - self.base) / Self::BYTES_PER_FIELD
    }

    fn mask(page: &Page) -> usize {
        1 << (page.address().raw() / Page::SIZE % (size_of::<usize>() * 8))
    }

    #[inline(always)]
    fn allocated(&self, page: &Page) -> bool {
        self.bitmap.lock()[self.index_of(page)] & Self::mask(page) != 0
    }
}

impl RegionPageAllocator for BitmapAllocator {
    fn new(area: &MmapEntry) -> Self {
        Self {
            bitmap: Spinlock::new(vec![0; area.size / Self::BYTES_PER_FIELD]),
            search_index: AtomicUsize::new(0),
            base: area.start,
            size: area.size / Self::BYTES_PER_FIELD * Self::BYTES_PER_FIELD,
        }
    }

    fn owns(&self, page: &Page) -> bool {
        self.base <= page.address() && page.address() < self.base + self.size
    }

    fn alloc(&self) -> Option<Page> {
        fn first_unset_bit(field: usize) -> usize {
            let ret: usize;
            unsafe {
                llvm_asm!("tzcnt $1, $0" : "=r"(ret) : "r"(!field));
            }
            ret
        }

        let mut bitmap_lock = self.bitmap.lock();
        let start_index = self.search_index.load(Ordering::Relaxed);
        let bitmap_search = &mut bitmap_lock[start_index..];

        for (index, field) in bitmap_search.iter_mut().enumerate() {
            let first_unset = if *field == 0 {
                0
            } else if *field == !0 {
                continue;
            } else {
                first_unset_bit(*field)
            };

            *field |= 1 << first_unset;
            self.search_index
                .store(start_index + index, Ordering::Relaxed);
            return Some(Page::new(
                self.address(start_index + index, first_unset),
                PageType::Allocated,
            ));
        }

        None
    }

    fn dealloc(&self, page: Page) {
        assert!(self.allocated(&page), "Double free.");
        let mut bitmap_lock = self.bitmap.lock();

        let index = self.index_of(&page);
        self.search_index.store(index, Ordering::Relaxed);

        bitmap_lock[index] ^= Self::mask(&page);
        forget(page);
    }
}

#[cfg(test)]
mod test {
    use super::{BitmapAllocator, Page, RegionPageAllocator};

    use crate::mem::map::{MmapEntry, MmapEntryType};
    use crate::mem::{size, Address, PhysicalAddress};

    const N_PAGES: usize = 2 * 64;

    fn make_allocator() -> BitmapAllocator {
        BitmapAllocator::new(&MmapEntry {
            entry_type: MmapEntryType::Usable,
            start: PhysicalAddress::new(1024 * size::KB),
            size: N_PAGES * Page::SIZE,
        })
    }

    #[test]
    fn alloc() {
        let allocator = make_allocator();
        let page = allocator.alloc().expect("Failed to allocate page.");
        assert!(allocator.allocated(&page));
        allocator.dealloc(page);
    }

    #[test]
    fn alloc_all() {
        let allocator = make_allocator();
        let mut pages = Vec::new();

        while let Some(page) = allocator.alloc() {
            pages.push(page);
        }

        assert!(allocator.alloc().is_none());
        assert_eq!(pages.len(), N_PAGES);

        pages.into_iter().for_each(|p| allocator.dealloc(p));
        allocator.dealloc(allocator.alloc().unwrap());
    }
}
