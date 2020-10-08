use alloc::alloc::{GlobalAlloc, Layout};
use alloc::vec::Vec;
use core::mem::{align_of, size_of};

use super::ALLOCATOR;

unsafe fn make_unchecked<T>() -> *mut T {
    ALLOCATOR.alloc(Layout::from_size_align(size_of::<T>(), align_of::<T>()).unwrap()) as *mut T
}

fn make<T>() -> *mut T {
    let ret = unsafe { make_unchecked::<T>() };
    assert!(!ret.is_null());
    ret
}

fn dealloc<T>(ptr: *mut T) {
    unsafe {
        ALLOCATOR.dealloc(
            ptr as *mut u8,
            Layout::from_size_align(size_of::<T>(), align_of::<T>()).unwrap(),
        )
    }
}

#[test]
fn alloc() {
    let ptr = make::<usize>();
}

#[test]
fn writable() {
    let ptr = make::<usize>();
    unsafe {
        let v = 424242424;
        ptr.write(v);
        assert_eq!(ptr.read(), v);
    }
}

#[test]
fn free() {
    let ptr = make::<usize>();
    unsafe { ptr.write(0x328542834) };
    dealloc(ptr);
}

#[test]
fn big_alloc() {
    let ptr = make::<[u16; 2000]>();
    let coll = unsafe { &mut *ptr };
    for (i, c) in coll.iter_mut().enumerate() {
        *c = i as u16;
    }
    for (i, c) in coll.iter().enumerate() {
        assert_eq!(*c, i as u16);
    }
    dealloc(ptr);
}

#[test]
fn alloc_all() {
    let mut ptrs = Vec::new();
    // Allocate all the memory.
    loop {
        let ptr = unsafe { make_unchecked::<u8>() };
        if ptr.is_null() {
            break;
        }
        ptrs.push(ptr);
    }

    // Can't allocate anymore.
    unsafe { assert!(make_unchecked::<u8>().is_null()) };

    // Free it all.
    ptrs.iter().for_each(|p| dealloc(*p));

    // Now it should be possible to allocate another u8.
    let ptr = make::<u8>();
    dealloc(ptr);
}
