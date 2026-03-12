/*
 * BITMAP ALLOCATOR: Physical area allocator.
 *
 * Copyright (c) 2026 Alex O'Brien <3541@3541.website>
 *
 * This file is part of Syzygy.
 *
 * Syzygy is free software: you can redistribute it and/or modify it under the
 * terms of version 3 the GNU General Public License as published by the Free
 * Software Foundation.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software. If not, see <https://www.gnu.org/licenses/>.
 */

use alloc::vec::Vec;
use core::{cmp::min, mem};
use log::{debug, info, trace};

use super::arch::MIN_PAGE_SIZE;
use crate::{
    mem::{
        DefaultAlloc, PhysicalAddress,
        phys::{PhysicalArea, types::PhysicalAllocation},
        types::RawPhysicalAddress,
    },
    sync::spin::{Spinlock, SpinlockGuard},
    util::InitOnce,
};
use common::{
    constants::MB,
    mmap::{Mmap, MmapEntryType},
};

struct Bitmap(Vec<usize, DefaultAlloc>);

impl Bitmap {
    const BITS_PER_FIELD: usize = mem::size_of::<usize>() * u8::BITS as usize;

    fn field_index(index: usize) -> usize {
        index / Self::BITS_PER_FIELD
    }

    fn bit_index(index: usize) -> usize {
        index % Self::BITS_PER_FIELD
    }

    fn mask(offset: usize, count: usize) -> usize {
        let count = min(count, Self::BITS_PER_FIELD - offset);
        let mask = if count == Self::BITS_PER_FIELD {
            usize::MAX
        } else {
            (1 << count) - 1
        };

        mask << offset
    }

    fn mark(&mut self, mut index: usize, mut count: usize) {
        while count > 0 {
            trace!("Marking {count} bits starting at index {index}.");

            let field_index = Self::field_index(index);
            let field = &mut self.0[field_index];
            let bit_index = Self::bit_index(index);
            let mask = Self::mask(bit_index, count);

            trace!("Field index: {field_index}, bit offset: {bit_index} => mask: {mask:#b}");

            assert_eq!(*field & mask, 0);
            *field |= mask;

            let marked = min(Self::BITS_PER_FIELD - bit_index, count);
            assert!(count >= marked);
            index += marked;
            count -= marked;
        }
    }

    fn clear(&mut self, mut index: usize, mut count: usize) {
        while count > 0 {
            trace!("Clearing {count} bits starting at index {index}.");

            let field_index = Self::field_index(index);
            let field = &mut self.0[field_index];
            let bit_index = Self::bit_index(index);
            let mask = Self::mask(bit_index, count);

            trace!("Field index: {field_index}, bit offset: {bit_index} => mask: {mask:#b}");

            assert_eq!(*field & mask, mask);
            *field &= !mask;

            let cleared = min(Self::BITS_PER_FIELD - bit_index, count);
            assert!(count >= cleared);
            index += cleared;
            count -= cleared;
        }
    }

    fn count_free_up_to(&self, mut index: usize, mut count: usize) -> usize {
        let mut free = 0;
        while count > 0 {
            trace!("Searching for {count} free bits starting at index {index}.");

            let field_index = Self::field_index(index);
            let field = &self.0[field_index];
            let bit_index = Self::bit_index(index);
            let mask = Self::mask(bit_index, count);

            trace!("Field index: {field_index}, bit offset: {bit_index} => mask: {mask:#b}");

            if field & mask != 0 {
                return free + (field & mask).trailing_zeros() as usize - bit_index;
            }

            let checked = min(Self::BITS_PER_FIELD - bit_index, count);
            assert!(count >= checked);
            index += checked;
            count -= checked;
            free += checked;
        }

        free
    }

    fn free_count(&self) -> usize {
        let mut res = 0usize;
        for field in &self.0 {
            res += field.count_zeros() as usize;
        }

        res
    }
}

pub struct BitmapAlloc {
    bitmap: Spinlock<Bitmap>,
    base: PhysicalAddress,
}

static ALLOC: InitOnce<BitmapAlloc> = InitOnce::new();

impl BitmapAlloc {
    fn new(map: Mmap) -> Self {
        let total_size = map.max_usable_range.end - map.max_usable_range.start;
        assert_eq!(total_size % MIN_PAGE_SIZE, 0);

        let page_count = total_size / MIN_PAGE_SIZE;
        let bitmap_size = (page_count + Bitmap::BITS_PER_FIELD - 1) / Bitmap::BITS_PER_FIELD;
        let bit_count = bitmap_size * Bitmap::BITS_PER_FIELD;
        let mut v = Vec::with_capacity_in(bitmap_size, DefaultAlloc::the());
        v.resize(bitmap_size, 0);
        let mut bitmap = Bitmap(v);
        let base = PhysicalAddress::new(RawPhysicalAddress(map.max_usable_range.start));

        debug!(
            "Bitmap covers {} pages, {} bits.",
            total_size / MIN_PAGE_SIZE,
            bitmap_size * Bitmap::BITS_PER_FIELD
        );

        for region in map.map {
            let start_phys = PhysicalAddress::new(RawPhysicalAddress(region.start_phys));
            assert!(start_phys >= base);
            assert_eq!(region.size % MIN_PAGE_SIZE, 0);
            assert_eq!(region.start_phys % MIN_PAGE_SIZE, 0);

            let count = region.size / MIN_PAGE_SIZE;
            let index = (start_phys - base) as usize / MIN_PAGE_SIZE;

            debug!(
                "Assigning {:?} region, {} frames to bitmap, at index {}.",
                region.entry_type, count, index
            );

            match region.entry_type {
                MmapEntryType::Usable => {
                    assert!(index < page_count)
                },
                MmapEntryType::Reserved
                | MmapEntryType::Kernel
                | MmapEntryType::ACPIReclaimable
                    if index < page_count =>
                {
                    bitmap.mark(index, count)
                },
                _ => {},
            }
        }

        if bit_count > page_count {
            debug!(
                "Marking {} unusable bits as reserved.",
                bit_count - page_count
            );
            bitmap.mark(page_count, bit_count - page_count);
        }

        Self {
            bitmap: Spinlock::new(bitmap),
            base,
        }
    }

    pub fn the() -> &'static Self {
        ALLOC.get()
    }

    pub fn free_size(&self) -> usize {
        self.bitmap.lock().free_count() * MIN_PAGE_SIZE
    }

    pub fn alloc_contiguous(&self, size: usize) -> Option<PhysicalAllocation> {
        let bit_count = (size + MIN_PAGE_SIZE - 1) / MIN_PAGE_SIZE;
        trace!("Allocating {size} bytes contiguous physical memory ({bit_count} bits).");

        let mut map = self.bitmap.lock();
        // TODO: cache a search index.
        let mut index = 0usize;
        loop {
            let field_index = Bitmap::field_index(index);
            trace!("Searching at bit index {index}, in field {field_index}.");
            if field_index >= map.0.len() {
                break;
            }

            let field = &map.0[field_index];
            trace!("Field: {field:#b}.");
            if *field == !0 || (*field != 0 && bit_count > Bitmap::BITS_PER_FIELD) {
                index = (field_index + 1) * Bitmap::BITS_PER_FIELD;
                continue;
            }

            let bit_index = Bitmap::bit_index(index);
            let masked_field = field & !Bitmap::mask(0, bit_index);
            let first_free = masked_field.trailing_ones() as usize;
            index = field_index * Bitmap::BITS_PER_FIELD + first_free;
            if map.count_free_up_to(index, bit_count) == bit_count {
                let area = PhysicalArea {
                    start: self.base + index * MIN_PAGE_SIZE,
                    size: bit_count * MIN_PAGE_SIZE,
                };
                trace!("{bit_count} bits free following, marking and returning {area:?}.");
                map.mark(index, bit_count);
                return Some(unsafe { PhysicalAllocation::new(area) });
            }

            let first_set = masked_field.trailing_zeros() as usize;
            index = field_index * Bitmap::BITS_PER_FIELD + first_set + 1;
        }

        trace!("Reached end without satisfying allocation.");
        None
    }

    pub fn try_alloc(&self, size: usize) -> impl Iterator<Item = PhysicalAllocation> {
        let bit_count = (size + MIN_PAGE_SIZE - 1) / MIN_PAGE_SIZE;
        trace!("Allocating {size} bytes physical memory ({bit_count} bits).");

        struct AllocIterator<'a> {
            bitmap: SpinlockGuard<'a, Bitmap>,
            base: PhysicalAddress,
            index: usize,
            count: usize,
        }

        impl Iterator for AllocIterator<'_> {
            type Item = PhysicalAllocation;

            fn next(&mut self) -> Option<PhysicalAllocation> {
                let count = loop {
                    let field_index = Bitmap::field_index(self.index);
                    let count = self.bitmap.count_free_up_to(self.index, self.count);
                    if count != 0 || field_index >= self.bitmap.0.len() {
                        break count;
                    }

                    self.index += 1
                };

                if count == 0 {
                    return None;
                }

                assert!(count <= self.count);
                let start = self.base + self.index * MIN_PAGE_SIZE;
                self.bitmap.mark(self.index, count);
                self.index += count;
                self.count -= count;
                Some(unsafe {
                    PhysicalAllocation::new(PhysicalArea {
                        start,
                        size: count * MIN_PAGE_SIZE,
                    })
                })
            }
        }

        AllocIterator {
            bitmap: self.bitmap.lock(),
            base: self.base,
            index: 0,
            count: bit_count,
        }
    }

    // SAFETY: A given allocation can only be freed once. If this is not called fromp Drop, the allocation must subsequently be forgotten.
    pub(super) unsafe fn free(&self, alloc: &mut PhysicalAllocation) {
        assert_eq!(alloc.start % MIN_PAGE_SIZE, 0);
        assert_eq!(alloc.size % MIN_PAGE_SIZE, 0);
        assert!(alloc.start >= self.base);
        let index = (alloc.start - self.base) as usize / MIN_PAGE_SIZE;
        let count = alloc.size / MIN_PAGE_SIZE;

        self.bitmap.lock().clear(index, count);
    }
}

pub(super) fn init(mmap: Mmap) {
    let overall_size = mmap.max_usable_range.end - mmap.max_usable_range.start;
    let res = BitmapAlloc::new(mmap);
    let free = res.free_size();
    assert!(free < overall_size);
    info!(
        "Initialized physical memory allocator. {}/{} MB usable.",
        free / MB,
        overall_size / MB
    );

    ALLOC.init(res);
}
