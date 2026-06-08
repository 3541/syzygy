/*
 * VIRT ALLOC: Virtual address space allocator.
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

use core::ops::Bound;

use alloc::collections::BTreeMap;
use log::trace;

use super::{arch::KERNEL_VMALLOC_AREA, types::VirtualArea};
use crate::{
    mem::{
        VirtualAddress, align_up, heap::DefaultAlloc, phys::MIN_PAGE_SIZE,
        virt::types::VirtualAllocation,
    },
    sync::spin::Spinlock,
    util::InitOnce,
};

pub struct VMAlloc {
    map: Spinlock<BTreeMap<VirtualAddress, usize, DefaultAlloc>>,
    area: VirtualArea,
}

static KERNEL_VMALLOC: InitOnce<VMAlloc> = InitOnce::new();

impl VMAlloc {
    unsafe fn new(area: VirtualArea) -> Self {
        assert!(area.size > 0);

        let mut map = BTreeMap::new_in(DefaultAlloc::the());
        map.insert(area.start, area.size);
        Self {
            map: Spinlock::new(map),
            area,
        }
    }

    // The kernel mappings are shared globally by mapping a single second-from-top level page table (PDP for amd64).
    // The kernel text/data are shared similarly, but with a separate entry.
    pub fn kernel() -> &'static Self {
        KERNEL_VMALLOC.get()
    }

    pub fn total_free(&self) -> usize {
        self.map.lock().iter().map(|(_, size)| *size).sum()
    }

    pub fn alloc(&self, size: usize) -> Option<VirtualAllocation> {
        let size = align_up(size, MIN_PAGE_SIZE);
        trace!("Allocating {} bytes of VM space.", size);

        let mut map = self.map.lock();
        let (addr, avail) = map.extract_if(.., |_, s| *s >= size).next()?;
        let res = unsafe { VirtualAllocation::unmapped(VirtualArea { start: addr, size }) };
        if avail == size {
            return Some(res);
        }

        map.insert(addr + size, avail - size);
        Some(res)
    }

    // SAFETY: Must ensure this is only called once. If not for drop() taking &mut, this would be by value.
    pub unsafe fn free(&self, alloc: &mut VirtualAllocation) {
        trace!(
            "Freeing VM allocation: {} ({} bytes).",
            alloc.start, alloc.size
        );
        assert!(
            self.area.contains_area(**alloc),
            "VM area {} ({} bytes) freed with wrong allocator.",
            alloc.start,
            alloc.size
        );
        assert!(alloc.size > 0);

        let mut map = self.map.lock();
        // SAFETY: I promise not to fuck it up.
        let mut entry = unsafe {
            map.lower_bound_mut(Bound::Excluded(&alloc.start))
                .with_mutable_key()
        };
        if let Some((&mut addr, size)) = entry.peek_prev() {
            assert!(addr < alloc.start);

            if addr + *size == alloc.start {
                *size += alloc.size;
                return;
            }
        }
        if let Some((addr, size)) = entry.peek_next() {
            assert!(alloc.end() <= *addr);

            if alloc.end() == *addr {
                // SAFETY: Previously checked that if there is a previous entry, it is less than this.
                *addr = alloc.start;
                *size += alloc.size;
                return;
            }
        }

        // No merging, so this is the insertion point.
        entry.insert_after(alloc.start, alloc.size).unwrap();
    }
}

pub(super) fn init() {
    KERNEL_VMALLOC.init(unsafe { VMAlloc::new(KERNEL_VMALLOC_AREA) });
}
