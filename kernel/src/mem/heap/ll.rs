use alloc::alloc::{GlobalAlloc, Layout};
use core::cmp::max;
use core::mem::{align_of, size_of};
use core::ptr;

use crate::mem::{Address, VirtualAddress};
use crate::util::sync::Spinlock;

// TODO: De-uglify.

pub struct LLNode {
    size: usize,
    next: Option<&'static mut LLNode>,
}

impl LLNode {
    pub const fn new(size: usize) -> LLNode {
        LLNode { size, next: None }
    }

    pub const fn fake(next: &'static mut LLNode) -> LLNode {
        LLNode {
            size: 0,
            next: Some(next),
        }
    }

    #[inline]
    fn validate(&self) {
        #[cfg(any(test, feature = "heap_validation"))]
        {
            unsafe {
                assert!(
                    VirtualAddress::new_unchecked(self as *const _ as usize).is_valid(),
                    "Pointer must be a canonical address."
                )
            };
            if let Some(ref next) = self.next {
                assert_ne!(
                    self as *const _, *next as *const _,
                    "Next cannot point to self."
                );
            }
        }
    }

    unsafe fn from_address(mut address: VirtualAddress, size: usize) -> &'static mut LLNode {
        let ret = &mut *address.as_mut_ptr::<LLNode>();
        ret.size = size;
        ret.next = None;
        ret.validate();

        ret
    }

    // Merge this node with an adjacent following node.
    fn merge(&mut self) {
        assert!(self.next.is_some());
        assert_eq!(
            self.end().next_aligned(align_of::<LLNode>()),
            VirtualAddress::from_ptr(*self.next.as_ref().unwrap())
        );

        let next = self.next.take().unwrap();

        self.size = next.end() - self.start();
        self.next = None;

        // Fixup next pointers and potentially call recursively if appropriate.
        if let Some(ref mut new_next) = next.next {
            self.insert_after(new_next);
        }
        self.validate();
    }

    fn insert_after(&mut self, other: &'static mut LLNode) {
        assert!(self.next.is_none() || other.next.is_none());
        if other.next.is_none() {
            other.next = self.next.take();
        }
        self.next = Some(other);
        self.validate();

        if self.end().next_aligned(align_of::<LLNode>())
            == VirtualAddress::from_ptr(*self.next.as_ref().unwrap())
        {
            self.merge();
        }
    }

    fn start(&self) -> VirtualAddress {
        self.validate();
        VirtualAddress::from_ptr(self)
    }

    fn end(&self) -> VirtualAddress {
        self.validate();
        self.start() + self.size
    }

    // Split a node into two pieces, one of which is of at least the desired
    // size. The list is modified such that the right node (if one is created)
    // points to the original node's child.
    fn split(
        &mut self,
        size: usize,
        align: usize,
    ) -> Option<(&'static mut LLNode, Option<&'static mut LLNode>)> {
        // The returned space must be able to fit a node, since it may be
        // returned as a single fragment.
        let end = self.start().next_aligned(align) + max(size_of::<LLNode>(), size);

        if end > self.end() {
            return None;
        }

        let other_start = end.next_aligned(align_of::<LLNode>());
        if other_start + size_of::<LLNode>() > self.end() {
            // This node fits the desired allocation, but is too small to have
            // enough space left over for another node to keep track of the new
            // free space.
            if end == self.end() {
                self.validate();
                let next = self.next.take();
                let ret = unsafe { LLNode::from_address(self.start(), self.size) };
                ret.next = next;
                Some((ret, None))
            } else {
                None
            }
        } else {
            let other = unsafe { LLNode::from_address(other_start, self.end() - end) };
            other.next = self.next.take();
            other.validate();
            // Careful: This actually overwrites self, so any self methods and
            // fields hereafter are actually coming from ret.
            let ret = unsafe { LLNode::from_address(self.start(), end - self.start()) };
            ret.validate();
            other.validate();
            Some((ret, Some(other)))
        }
    }
}

pub struct LLAlloc {
    // head is a dummy node which has size: 0.
    head: Spinlock<LLNode>,
}

impl LLAlloc {
    // NOTE: This does not set the first node's size correctly, so this must be
    // ensured by the caller. The easiest way to do this is probably to just
    // prefill the slice nodes.
    pub const unsafe fn from_slice(backing: &'static mut [LLNode]) -> LLAlloc {
        assert!(
            backing.len() >= size_of::<LLNode>(),
            "LLAlloc backing must fit at least one LLNode."
        );
        LLAlloc {
            head: Spinlock::new(LLNode::fake(&mut backing[0])),
        }
    }

    // This should only be used in such a way that it genuinely takes ownership
    // of the region described by (start, size).
    unsafe fn push(&self, start: VirtualAddress, size: usize) {
        assert!(size >= size_of::<LLNode>());
        assert!(start.is_aligned(align_of::<LLNode>()));

        let mut lock = self.head.lock();
        let new_node = LLNode::from_address(start, size);
        let mut current = &mut *lock;

        // Find the first node after the appropriate insertion point, and then
        // insert after the previous.
        //        while let Some(ref mut current) = prev.next {
        loop {
            if (current.next.is_some() && current.next.as_mut().unwrap().start() > new_node.start())
                || current.next.is_none()
            {
                current.insert_after(new_node);
                break;
            }
            if let Some(ref mut node) = current.next {
                current = node;
            } else {
                break;
            }
        }
    }

    // Get a node for allocation of the specified size and alignment,
    // reinserting any free space into the list.
    fn get_node(&self, size: usize, align: usize) -> Option<&'static mut LLNode> {
        let mut lock = self.head.lock();
        let mut prev = &mut *lock;

        while let Some(ref mut current) = prev.next {
            match current.split(size, align) {
                Some((node, None)) => {
                    prev.next = None;
                    if node.next.is_some() {
                        prev.insert_after(node.next.take().unwrap());
                    }
                    return Some(node);
                }
                Some((node, Some(leftover))) => {
                    prev.next = None;
                    prev.insert_after(leftover);
                    return Some(node);
                }
                None => {}
            }

            prev = prev.next.as_mut().unwrap();
        }

        None
    }
}

unsafe impl GlobalAlloc for LLAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if let Some(node) = self.get_node(layout.size(), layout.align()) {
            let ret: *mut u8 = node.start().next_aligned(layout.align()).as_mut_ptr();
            // Zero out LL info.
            ret.write_bytes(0, size_of::<LLNode>());
            ret
        } else {
            ptr::null_mut()
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        ptr.write_bytes(0, size_of::<LLNode>());
        self.push(
            VirtualAddress::from_ptr(ptr),
            max(size_of::<LLNode>(), layout.size()),
        )
    }
}

// So that super::ALLOCATOR works.
unsafe impl GlobalAlloc for &LLAlloc {
    // Just forward everything to the actual implementation.
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        (*self).alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        (*self).dealloc(ptr, layout);
    }
}
