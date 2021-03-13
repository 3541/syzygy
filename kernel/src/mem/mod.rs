//! # Memory management.

pub mod arch;
pub mod heap;
pub mod map;
mod paging;
pub mod phys;
mod region;
mod types;

pub use phys::{Page, PageAllocator};
pub use region::VirtualRegion;
pub use types::{
    size, Address, PhysicalAddress, RawPhysicalAddress, RawVirtualAddress, VirtualAddress,
};
