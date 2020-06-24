mod heap;
pub mod paging;
pub mod phys;
pub mod region;
pub mod size;
//mod watermark_frame_allocator;

use core::fmt;
use core::iter::Step;
use core::marker::Sized;
use core::ops::{Add, AddAssign, Deref, Sub};

pub use heap::{init_allocator, init_heap};
pub use phys::alloc::{FrameAllocator, PhysicalMemoryManager};
pub use phys::{Frame, PhysicalMemory, PHYSICAL_ALLOCATOR};
pub use region::VirtualRegion;
//pub use watermark_frame_allocator::WatermarkFrameAllocator;

pub type RawPhysicalAddress = usize;
pub type RawVirtualAddress = usize;

pub trait Address: Deref<Target = usize> + Sized + Eq {
    const SIGN_EX_INVALID_BASE: usize = 0x0000_8000_0000_0000;
    const SIGN_EX_INVALID_TOP: usize = 0xFFFF_8000_0000_0000;

    fn new(addr: usize) -> Self {
        let ret = unsafe { Self::new_unchecked(addr) };
        assert!(
            ret.is_valid(),
            "Invalid address! Sign extension set incorrectly."
        );
        ret
    }

    unsafe fn new_unchecked(addr: usize) -> Self;

    #[inline]
    unsafe fn zero() -> Self {
        Self::new_unchecked(0)
    }

    #[inline]
    fn is_aligned(&self, align: usize) -> bool {
        **self % align == 0
    }

    #[inline]
    fn previous_aligned(&self, align: usize) -> Self {
        Self::new(**self & !(align - 1))
    }

    #[inline]
    fn next_aligned(&self, align: usize) -> Self {
        Self::new(**self + align - 1).previous_aligned(align)
    }

    #[inline]
    fn is_zero(&self) -> bool {
        *self == unsafe { Self::zero() }
    }

    #[inline]
    fn is_valid(&self) -> bool {
        **self < Self::SIGN_EX_INVALID_BASE || **self >= Self::SIGN_EX_INVALID_TOP
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, Ord, Eq, PartialOrd, PartialEq)]
pub struct PhysicalAddress(RawPhysicalAddress);
impl PhysicalAddress {
    pub const unsafe fn new_const(addr: RawPhysicalAddress) -> Self {
        Self(addr)
    }

    pub const fn raw(&self) -> RawPhysicalAddress {
        self.0
    }
}

impl Address for PhysicalAddress {
    unsafe fn new_unchecked(addr: RawPhysicalAddress) -> Self {
        Self::new_const(addr)
    }
}

impl fmt::Display for PhysicalAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PhysicalAddress(0x{:x})", **self)
    }
}

impl Deref for PhysicalAddress {
    type Target = RawPhysicalAddress;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Add<usize> for PhysicalAddress {
    type Output = PhysicalAddress;

    fn add(self, rhs: usize) -> Self {
        PhysicalAddress::new(
            self.checked_add(rhs)
                .expect("Physical address addition overflowed"),
        )
    }
}

impl AddAssign<usize> for PhysicalAddress {
    fn add_assign(&mut self, rhs: usize) {
        self.0 = self.0 + rhs;
    }
}

impl Add<PhysicalAddress> for PhysicalAddress {
    type Output = PhysicalAddress;

    fn add(self, rhs: PhysicalAddress) -> Self {
        self + *rhs
    }
}

impl Sub<usize> for PhysicalAddress {
    type Output = PhysicalAddress;

    fn sub(self, rhs: usize) -> Self {
        PhysicalAddress::new(
            self.checked_sub(rhs)
                .expect("Physical address subtraction underflowed"),
        )
    }
}

impl Sub<PhysicalAddress> for PhysicalAddress {
    type Output = usize;

    fn sub(self, rhs: PhysicalAddress) -> usize {
        *(self - *rhs)
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, Ord, Eq, PartialOrd, PartialEq)]
pub struct VirtualAddress(RawVirtualAddress);
impl VirtualAddress {
    pub const PAGE_ADDR_INDEX_SHIFT: usize = 9;
    const PAGE_ADDR_INDEX_MASK: usize = (1 << Self::PAGE_ADDR_INDEX_SHIFT) - 1;
    const PAGE_ADDR_OFFSET_SHIFT: usize = 12;
    const PAGE_ADDR_OFFSET_MASK: usize = (1 << Self::PAGE_ADDR_OFFSET_SHIFT) - 1;

    pub const unsafe fn new_const(addr: RawVirtualAddress) -> Self {
        Self(addr)
    }

    pub const fn raw(&self) -> RawVirtualAddress {
        self.0
    }

    pub const fn pml4_index(&self) -> usize {
        self.table_index(3)
    }

    pub const fn pdp_index(&self) -> usize {
        self.table_index(2)
    }

    pub const fn pd_index(&self) -> usize {
        self.table_index(1)
    }

    pub const fn pt_index(&self) -> usize {
        self.table_index(0)
    }

    // NOTE: 0-indexed (PT is 0, PML4 is 3)
    const fn table_index(&self, n: usize) -> usize {
        // NOTE: it should be okay to use OFFSET_SHIFT like this, even though it's
        // sort of broken for larger pages, because the total offset is still the same
        // if we want some specific table. e.g., PML4 index is always at the same place.
        (self.raw() >> (Self::PAGE_ADDR_OFFSET_SHIFT + Self::PAGE_ADDR_INDEX_SHIFT * n))
            & Self::PAGE_ADDR_INDEX_MASK
    }

    pub const fn offset_into_frame(&self) -> usize {
        self.raw() & Self::PAGE_ADDR_OFFSET_MASK
    }
}

impl Address for VirtualAddress {
    unsafe fn new_unchecked(addr: RawVirtualAddress) -> Self {
        Self::new_const(addr)
    }
}

impl fmt::Display for VirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "VirtualAddress(0x{:x})", **self)
    }
}

impl Deref for VirtualAddress {
    type Target = RawVirtualAddress;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Add<usize> for VirtualAddress {
    type Output = VirtualAddress;

    fn add(self, rhs: usize) -> Self {
        VirtualAddress::new(
            self.checked_add(rhs)
                .expect("Virtual address addition overflowed"),
        )
    }
}

impl Sub<VirtualAddress> for VirtualAddress {
    type Output = usize;

    fn sub(self, rhs: VirtualAddress) -> usize {
        self.checked_sub(*rhs)
            .expect("Virtual address subtraction underflowed")
    }
}

impl Add<VirtualAddress> for VirtualAddress {
    type Output = VirtualAddress;

    fn add(self, rhs: VirtualAddress) -> Self {
        self + *rhs
    }
}

unsafe impl Step for VirtualAddress {
    fn steps_between(start: &VirtualAddress, end: &VirtualAddress) -> Option<usize> {
        end.checked_sub(**start)
    }

    fn forward_checked(start: VirtualAddress, count: usize) -> Option<VirtualAddress> {
        Some(VirtualAddress::new(start.checked_add(count)?))
    }

    fn backward_checked(start: VirtualAddress, count: usize) -> Option<VirtualAddress> {
        Some(VirtualAddress::new(start.checked_sub(count)?))
    }
}
