/*
 * VM MAP: Paging.
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

mod arch;

use alloc::boxed::Box;

use crate::{
    mem::{DefaultAlloc, phys::PhysicalAllocation, virt::types::VirtualArea},
    sync::spin::Spinlock,
};

#[derive(Debug)]
pub struct VirtualMapping {
    area: VirtualArea,
    // TODO: Refcounting.
    backing: PhysicalAllocation,
}

impl VirtualMapping {
    pub fn area(&self) -> &VirtualArea {
        &self.area
    }
}

pub struct PagerImpl;
pub struct Pager(Spinlock<PagerImpl>);

impl Pager {
    // SAFETY: Must be called only once for a given set of tables.
    pub(super) unsafe fn create_for_current() {
        let res = Box::leak(Box::new_in(
            Pager(Spinlock::new(PagerImpl)),
            DefaultAlloc::the(),
        ));
        // store_in_zero_page_entry(res as *const _ as *const ());
    }

    pub fn current() -> &'static Self {
        todo!()
    }
}
