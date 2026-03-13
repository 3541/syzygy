/*
 * LL: Simple first-fit linked-list heap allocator.
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

use core::{
    alloc::{AllocError, Allocator, Layout},
    cmp::max,
    mem,
    ptr::{self, NonNull},
};

use log::trace;

use crate::{
    mem::{VirtualAddress, align_up},
    sync::spin::Spinlock,
};
use common::constants::MB;

const HEAP_GROWTH_INCREMENT: usize = 1 * MB;

pub(super) struct Node {
    size: usize,
    next: *mut Node,
}

impl Node {
    pub const fn new(size: usize) -> Self {
        Self {
            size,
            next: ptr::null_mut(),
        }
    }

    fn start(&self) -> VirtualAddress {
        VirtualAddress::of(self)
    }

    fn end(&self) -> VirtualAddress {
        self.start() + self.size
    }

    fn block_size(size: usize) -> usize {
        max(mem::size_of::<Node>(), size)
    }

    fn try_split(&mut self, layout: Layout) -> bool {
        let end = self.start().align_up(layout.align()) + Self::block_size(layout.size());
        if end > self.end() {
            return false;
        }
        if end == self.end() {
            return true;
        }

        let next_start = end.align_up(mem::align_of::<Node>());
        let next_size = self.end() - next_start;
        if next_size < mem::size_of::<Node>() as isize {
            return true;
        }
        let next_size = next_size as usize;

        let new_size = end - self.start();
        assert!(new_size > 0);
        self.size = new_size as usize;
        let other = next_start.as_mut_ptr::<Node>();
        unsafe {
            other.as_mut_unchecked().size = next_size;
            other.as_mut_unchecked().next = self.next;
        }
        self.next = other;

        return true;
    }
}

pub(super) struct LLAlloc(Spinlock<*mut Node>);

unsafe impl Send for LLAlloc {}
unsafe impl Sync for LLAlloc {}

impl LLAlloc {
    pub(super) const fn from_slice(data: *mut [Node]) -> Self {
        assert!(!data.is_null());
        assert!(
            data.len() >= mem::size_of::<Node>(),
            "Storage for LLAlloc must fit at least one block."
        );

        unsafe {
            let first = &data.as_ref().unwrap()[0];
            assert!(first.size == data.len() * mem::size_of::<Node>());
            assert!(first.next.is_null());
        }

        Self(Spinlock::new(data.cast()))
    }

    #[cfg(test)]
    fn total_free(&self) -> usize {
        let head = self.0.lock();
        let mut current = *head;
        let mut total = 0usize;
        while !current.is_null() {
            unsafe {
                let cur = current.as_ref().unwrap();
                total += cur.size;
                current = cur.next;
            }
        }

        total
    }

    #[cfg(test)]
    fn max_node_size(&self) -> usize {
        let head = self.0.lock();
        let mut current = *head;
        let mut res = 0usize;
        while !current.is_null() {
            unsafe {
                let cur = current.as_ref().unwrap();
                res = max(res, cur.size);
                current = cur.next;
            }
        }

        res
    }

    fn try_alloc(&self, layout: Layout) -> Option<NonNull<[u8]>> {
        trace!(
            "Allocating {} bytes ({} aligned).",
            layout.size(),
            layout.align()
        );
        assert!(layout.align() <= mem::align_of::<Node>());

        let mut head = self.0.lock();
        if head.is_null() {
            return None;
        }

        let mut prev = *head;
        let mut current = prev;

        while !current.is_null() {
            let fits = unsafe { current.as_mut_unchecked().try_split(layout) };
            if !fits {
                prev = current;
                current = unsafe { current.as_ref_unchecked().next };
                continue;
            }

            unsafe {
                let current = current.as_mut_unchecked();
                let next = current.next;
                let size = current.size;
                current.next = ptr::null_mut();
                current.size = 0usize;

                if prev != current as *mut _ {
                    prev.as_mut_unchecked().next = next;
                } else {
                    *head = next;
                }

                let res = align_up(current as *mut _ as usize, layout.align()) as *mut u8;

                return Some(NonNull::slice_from_raw_parts(
                    NonNull::new_unchecked(res),
                    size,
                ));
            }
        }

        None
    }
}

unsafe impl Allocator for LLAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        if let Some(res) = self.try_alloc(layout) {
            return Ok(res);
        }

        trace!("Insufficient space available. Allocating more physical memory.");
        let size = max(HEAP_GROWTH_INCREMENT, layout.size());
        todo!();

        self.try_alloc(layout).ok_or(AllocError)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        let actual_size = Node::block_size(layout.size());
        let mut node = ptr.cast::<Node>();
        unsafe {
            node.write(Node::new(actual_size));
        }

        // TODO: Keep sorted, merge as required.
        let mut head = self.0.lock();
        unsafe {
            if let Some(head) = head.as_mut() {
                node.as_mut().next = head;
            }
        }
        *head = node.as_ptr();
    }
}

#[cfg(test)]
mod test {
    use super::{LLAlloc, Node};
    use core::mem;
    use std::boxed::Box;

    struct Victim {
        _data: Box<[Node]>,
        victim: LLAlloc,
    }

    type TestBox<'a, T> = Box<T, &'a LLAlloc>;

    impl Victim {
        fn new() -> Self {
            let mut data = Box::from([const { Node::new(1024 * mem::size_of::<Node>()) }; 1024]);
            let data_ptr: *mut [Node] = Box::as_mut(&mut data);
            Self {
                _data: data,
                victim: LLAlloc::from_slice(data_ptr),
            }
        }
    }

    #[test]
    fn alloc() {
        let victim = Victim::new();
        let initial_size = victim.victim.total_free();

        {
            let _ptr = TestBox::<usize>::new_uninit_in(&victim.victim);
            assert!(victim.victim.total_free() < initial_size);
        }

        assert_eq!(victim.victim.total_free(), initial_size);
    }

    #[test]
    fn writable() {
        let victim = Victim::new();
        let v = 424242424;
        let ptr = TestBox::<usize>::new_in(v, &victim.victim);
        assert_eq!(*ptr, v);
    }

    fn big_alloc_impl(victim: &Victim) {
        let mut ptr = TestBox::<[u16; 2000]>::new_in([0u16; 2000], &victim.victim);
        for (i, c) in ptr.iter_mut().enumerate() {
            *c = i as u16;
        }
        for (i, c) in ptr.iter().enumerate() {
            assert_eq!(*c, i as u16);
        }
    }

    #[test]
    fn big_alloc() {
        let victim = Victim::new();
        big_alloc_impl(&victim);
    }

    fn alloc_all_impl(victim: &Victim) {
        let initial_size = victim.victim.total_free();
        let mut data = Vec::new();

        while let Ok(b) = TestBox::<u8>::try_new_in(0, &victim.victim) {
            data.push(b);
        }
        assert!(TestBox::<u8>::try_new_in(0, &victim.victim).is_err());
        assert_eq!(victim.victim.total_free(), 0);

        data.clear();
        assert!(TestBox::<u8>::try_new_in(0, &victim.victim).is_ok());

        assert_eq!(victim.victim.total_free(), initial_size);
    }

    #[test]
    fn alloc_all() {
        let victim = Victim::new();
        alloc_all_impl(&victim);
    }

    // TODO:
    #[should_panic]
    #[test]
    fn coalesce() {
        let victim = Victim::new();
        let initial_size = victim.victim.total_free();

        // Cause fragmentation.
        alloc_all_impl(&victim);

        assert_eq!(victim.victim.total_free(), initial_size);
        assert_eq!(victim.victim.max_node_size(), initial_size);

        // Make a big allocation.
        big_alloc_impl(&victim);
    }
}
