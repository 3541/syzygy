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

    fn allocated(&self, bitmap: &mut [usize], page: &Page) -> bool {
        bitmap[self.index_of(page)] & Self::mask(page) != 0
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
        let bitmap_search = &mut bitmap_lock[self.search_index.load(Ordering::Relaxed)..];

        for (index, field) in bitmap_search.iter_mut().enumerate() {
            let first_unset = if *field == 0 {
                0
            } else if *field == !0 {
                continue;
            } else {
                first_unset_bit(*field)
            };

            *field |= 1 << first_unset;
            self.search_index.store(index, Ordering::Relaxed);
            return Some(Page::new(
                self.address(index, first_unset),
                PageType::Allocated,
            ));
        }

        None
    }

    fn dealloc(&self, page: Page) {
        let mut bitmap_lock = self.bitmap.lock();
        assert!(self.allocated(&mut bitmap_lock, &page), "Double free.");

        let index = self.index_of(&page);
        self.search_index.store(index, Ordering::Relaxed);

        bitmap_lock[index] ^= Self::mask(&page);
        forget(page);
    }
}
