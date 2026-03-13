/*
 * HEAP: General-purpose kernel heap.
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

mod ll;

use core::{
    alloc::{AllocError, Allocator, GlobalAlloc, Layout},
    mem::size_of,
    ptr::{self, NonNull},
};

use common::constants::KB;
use ll::{LLAlloc, Node};

const INIT_HEAP_SIZE: usize = 512 * KB;
const INIT_HEAP_NODE_COUNT: usize = INIT_HEAP_SIZE / size_of::<Node>();
static mut INIT_HEAP: [Node; INIT_HEAP_NODE_COUNT] =
    [const { Node::new(INIT_HEAP_SIZE) }; INIT_HEAP_NODE_COUNT];
// SAFETY: Mutable static only accessed here, and thereafter protected by LLAlloc's lock.
static HEAP: LLAlloc = LLAlloc::from_slice(&raw mut INIT_HEAP);

#[derive(Copy, Clone)]
pub struct DefaultAlloc(());

impl DefaultAlloc {
    pub fn the() -> Self {
        DefaultAlloc(())
    }
}

unsafe impl Allocator for DefaultAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        HEAP.allocate(layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        unsafe { HEAP.deallocate(ptr, layout) }
    }
}

struct DummyGlobalAlloc;

unsafe impl GlobalAlloc for DummyGlobalAlloc {
    unsafe fn alloc(&self, _: Layout) -> *mut u8 {
        ptr::null_mut()
    }

    unsafe fn dealloc(&self, _: *mut u8, _: Layout) {
        panic!("Global allocator not supported.");
    }
}

#[cfg_attr(not(test), global_allocator)]
static DUMMY_GLOBAL_ALLOC: DummyGlobalAlloc = DummyGlobalAlloc;
