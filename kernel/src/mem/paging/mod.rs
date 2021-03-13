//! Paging and virtual memory. The traits here are guides for architecture-specific implementations.

mod arch;

use alloc::vec::Vec;
use core::mem::forget;
use core::ops::{Drop, Index, IndexMut};

use log_crate::warn;

use crate::mem::{Page, VirtualAddress, VirtualRegion};

/// Returned from functions which change memory mappings. Causes a panic if not consumed by
/// flushing.
#[must_use = "Mapping changes must be flushed."]
struct Flush(VirtualAddress);

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

/// Behavior of a page table.
trait TPageTable: Index<usize> + IndexMut<usize> {
    /// Create a blank page table in the given page.
    fn from_page(p: Page) -> Self;
    /// Zero the table.
    fn zero(&mut self);
}

/// Behavior of the top-level page table.
trait TPrimaryTable {
    fn the() -> Self;
    fn map(from: VirtualAddress, to: Page);
    fn map_range(from: VirtualRegion, to: Vec<Page>);
}
