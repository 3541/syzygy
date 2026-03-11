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
use log::{debug, trace};

use super::arch::MIN_PAGE_SIZE;
use crate::{
    mem::{DefaultAlloc, PhysicalAddress, types::RawPhysicalAddress},
    sync::spin::Spinlock,
};
use common::mmap::{Mmap, MmapEntryType};

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
            let field = &mut self.0[Self::field_index(index)];
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
    size: usize,
}

impl BitmapAlloc {
    pub(super) fn new(map: Mmap) -> Self {
        let total_size = map.max_usable_range.end - map.max_usable_range.start;
        assert_eq!(total_size % MIN_PAGE_SIZE, 0);

        let page_count =total_size / MIN_PAGE_SIZE ;
        let bitmap_size =
            (page_count + Bitmap::BITS_PER_FIELD - 1) / Bitmap::BITS_PER_FIELD;
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
            debug!("Marking {} unusable bits as reserved.", bit_count - page_count);
            bitmap.mark(page_count, bit_count - page_count);
        }

        Self {
            bitmap: Spinlock::new(bitmap),
            base,
            size: total_size,
        }
    }

    pub fn free_size(&self) -> usize {
        self.bitmap.lock().free_count() * MIN_PAGE_SIZE
    }
}
