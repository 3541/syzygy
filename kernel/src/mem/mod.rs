//! # Memory management.

pub mod arch;
pub mod heap;
pub mod map;
pub mod phys;
mod types;
pub mod virt;

pub use phys::{Page, PageAllocator, PageRef, PageType};
pub use types::{
    size, Address, PhysicalAddress, RawPhysicalAddress, RawVirtualAddress, VirtualAddress,
};
pub use virt::{VirtualRange, VirtualRegion};
