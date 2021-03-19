//! Paging and virtual memory. The traits here are guides for architecture-specific implementations.

mod arch;
mod region;

pub use arch::ActivePrimaryTable;
pub use region::{VirtualRange, VirtualRegion};

use alloc::vec::Vec;
use core::marker::PhantomData;
use core::mem::forget;
use core::ops::Drop;

use bitflags::bitflags;
use log_crate::warn;

use crate::consts::image;
use crate::mem::{Page, PageRef, PhysicalAddress, VirtualAddress};

/// Returned from functions which change memory mappings. Causes a panic if not consumed by
/// flushing.
#[must_use = "Mapping changes must be flushed."]
pub struct Flush(VirtualAddress);

impl Flush {
    fn flush(self) {
        arch::flush_mapping(self.0);
        forget(self);
    }
}

impl Drop for Flush {
    fn drop(&mut self) {
        panic!("Un-flushed mapping change at {}.", self.0);
    }
}

struct Unflushed<'a, T> {
    target: *mut T,
    flush: Flush,
    lt: PhantomData<&'a mut T>,
}

impl<'a, T> Unflushed<'a, T> {
    /// SAFETY: Caller must guarantee the target refers to an object which will become a valid
    /// reference once the mapping is flushed.
    unsafe fn from(flush: Flush) -> Self {
        Self {
            target: flush.0.as_mut_ptr(),
            flush,
            lt: PhantomData,
        }
    }

    /// SAFETY: Caller must guarantee that `target` refers to an object which will become a valid
    /// reference once the mapping is flushed.
    unsafe fn new(target: VirtualAddress) -> Self {
        Self::from(Flush(target))
    }

    fn flush(self) -> &'a mut T {
        let Unflushed { target, flush, .. } = self;
        flush.flush();
        // SAFETY: Persuant to the guarantees required in `new`, this is now safe.
        unsafe { &mut *target }
    }
}

/// Used to consume Flushes when it is desirable to simply flush the entire TLB.
#[must_use = "TLB flush must be executed."]
struct FlushAll(bool);

impl FlushAll {
    fn new() -> Self {
        Self(false)
    }

    fn consume(&mut self, f: Flush) {
        self.0 = true;
        forget(f);
    }

    fn flush(self) {
        if self.0 {
            arch::flush_all_mappings();
        } else {
            warn!("Unnecessary TLB flush.");
        }

        forget(self);
    }
}

impl Drop for FlushAll {
    fn drop(&mut self) {
        if self.0 {
            panic!("Multiple unflushed mappings.");
        } else {
            warn!("Dropping unused TLB flush.");
        }
    }
}

bitflags! {
    pub struct MappingFlags: usize {
        const WRITABLE = 1 << 1;
        const USER = 1 << 2;
        const NX = 1 << 63;
    }
}

/// Behavior of the top-level page table.
pub trait TActivePrimaryTable {
    type Inactive;
    /// This function probably doesn't enforce ownership constraints. It should be called /once/ in
    /// a thread's lifetime, and then stored somewhere reasonable.
    unsafe fn current_active() -> Self;
    fn map(&self, from: VirtualAddress, to: &mut Page, flags: MappingFlags) -> Flush;
    unsafe fn unmap(&self, addr: VirtualAddress) -> Flush;
    //    fn map_range(&self, from: VirtualRegion, to: Vec<Page>) -> Vec<Flush>;
    fn with(&self, t: &mut Self::Inactive, f: impl FnOnce(&Self));
    unsafe fn swap(&self, other: Self::Inactive) -> Self::Inactive;
    fn translate(&self, addr: VirtualAddress) -> Option<PhysicalAddress>;
    fn translate_range(&self, range: VirtualRange) -> Option<Vec<Option<PageRef>>>;
}

pub fn init() {
    // SAFETY: This is the only reader of the bootstrap tables.
    let root = unsafe { ActivePrimaryTable::current_active() };

    let kernel_image = [
        (image::text_range(), MappingFlags::empty()),
        (
            image::data_range(),
            MappingFlags::NX | MappingFlags::WRITABLE,
        ),
        (
            image::bss_range(),
            MappingFlags::NX | MappingFlags::WRITABLE,
        ),
        (image::rodata_range(), MappingFlags::NX),
        (image::tdata_range(), MappingFlags::NX),
        (image::tbss_range(), MappingFlags::NX),
    ];

    todo!()
}
