//! Paging and virtual memory. The traits here are guides for architecture-specific implementations.

mod arch;
mod region;

pub use arch::{ActiveRootTable, InactiveRootTable};
pub use region::{VirtualRange, VirtualRegion};

use alloc::vec;
use alloc::vec::Vec;
use core::mem::forget;
use core::ops::{Deref, Drop};

use bitflags::bitflags;
use log_crate::{trace, warn};

use crate::consts::{image, vga_text_virt_address, VGA_TEXT_PHYS_ADDRESS};
use crate::mem::{
    Address, Page, PageAllocator, PageRef, PageType, PhysicalAddress, VirtualAddress,
};
use region::VirtualBacking;

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

struct Unflushed<T: Deref> {
    inner: T,
    flush: Flush,
}

impl<T: Deref> Unflushed<T> {
    /// SAFETY: Caller must guarantee the target refers to an object which will become valid once
    /// the mapping is flushed.
    unsafe fn new(inner: T, flush: Flush) -> Self {
        Self { inner, flush }
    }

    fn flush(self) -> T {
        let Unflushed { inner, flush } = self;
        flush.flush();
        // SAFETY: Persuant to the guarantees required in `new`, this is now safe.
        inner
    }
}

impl<T> Unflushed<&mut T> {
    /// SAFETY: Caller must guarantee that `target` refers to an object which will become a valid
    /// reference once the mapping is flushed.
    unsafe fn from_address(target: VirtualAddress) -> Self {
        Self::new(&mut *target.as_mut_ptr(), Flush(target))
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

    fn consume_all(&mut self, mut f: Vec<Flush>) {
        if f.len() == 0 {
            return;
        }

        self.0 = true;
        f.drain(0..).for_each(forget);
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
        const EXECUTABLE = 1 << 63;
    }
}

/// Behavior of an inactive top-level page table. This trait and the following are not actually
/// required, but are useful as a source of truth betweeen architectural implementations.
pub trait InactiveRoot {
    fn new(p: Page) -> Self;
    fn phys_address(&self) -> PhysicalAddress;
}

/// Behavior of the top-level page table.
pub trait ActiveRoot {
    type Inactive: InactiveRoot;
    fn map(&self, from: VirtualAddress, to: &Page, flags: MappingFlags) -> Flush;
    fn map_range(
        &self,
        from: VirtualRange,
        to: Vec<Option<&Page>>,
        flags: MappingFlags,
    ) -> Vec<Flush>;
    unsafe fn unmap(&self, addr: VirtualAddress) -> Flush;
    /// For the duration of the given callback function, the page table mappings are altered as if
    /// `t` were the root table, thus allowing it to be written through the recursive mapping.
    fn with(&self, t: &mut Self::Inactive, f: impl FnOnce(&Self));
    unsafe fn swap(&self, other: Self::Inactive) -> Self::Inactive;
    fn translate(&self, addr: VirtualAddress) -> Option<PhysicalAddress>;
    fn translate_range(&self, range: VirtualRange) -> Option<Vec<Option<PageRef>>>;
}

pub fn init(_slide: usize) -> ActiveRootTable {
    let root_table = arch::init();
    trace!("Got current active table.");

    let phys_base = image::base() - image::LOAD_BASE;
    let offset = image::base().raw();

    let kernel_image: Vec<_> = vec![
        (image::text_range(), MappingFlags::EXECUTABLE),
        (image::data_range(), MappingFlags::WRITABLE),
        (image::bss_range(), MappingFlags::WRITABLE),
        (image::rodata_range(), MappingFlags::empty()),
        (image::tdata_range(), MappingFlags::empty()),
        (image::tbss_range(), MappingFlags::empty()),
    ]
    .drain(0..)
    .filter(|(r, _)| r.size() > 0)
    .map(|(r, f)| {
        let backing = r
            .into_iter()
            // SAFETY: Kernel sections are guaranteed to exist and be otherwise un-owned.
            .map(|a| unsafe {
                VirtualBacking::Present(Page::new(
                    PhysicalAddress::new(phys_base + a.raw() - offset),
                    PageType::Unallocated,
                ))
            })
            .collect();
        unsafe { VirtualRegion::create(r, backing, f) }
    })
    .collect();
    trace!("Translated kernel image sections.");

    let mut new_table = InactiveRootTable::new(
        PageAllocator::the()
            .alloc()
            .expect("Unable to allocate page for new page table."),
    );
    trace!("Created new root table.");

    root_table.with(&mut new_table, |root| {
        trace!("Mapping kernel regions.");
        let mut flush = FlushAll::new();
        for region in kernel_image {
            flush.consume_all(region.map(root));
        }

        flush.consume(root.map(
            vga_text_virt_address(),
            unsafe { &Page::new(VGA_TEXT_PHYS_ADDRESS, PageType::Unallocated) },
            MappingFlags::WRITABLE,
        ));

        flush.flush();
    });
    trace!("Mapped kernel in new table.");

    let old_table = unsafe { root_table.swap(new_table) };
    forget(old_table);
    trace!("Swapped to new table.");
    // This is now actually the new table.
    root_table
}
