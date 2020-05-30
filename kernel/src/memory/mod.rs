mod alloc;
mod heap;
pub mod paging;
pub mod phys;
mod region;
//mod watermark_frame_allocator;

use core::fmt;
use core::marker::Sized;
use core::ops::{Add, AddAssign, Deref, Sub};

pub use self::alloc::{add_heap, init_allocator};
pub use heap::init_heap;
pub use phys::alloc::{FrameAllocator, GlobalFrameAllocator, FRAME_ALLOCATOR};
pub use phys::Frame;
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
    fn prev_aligned_addr(&self, align: usize) -> Self {
        Self::new(**self & !(align - 1))
    }

    #[inline]
    fn next_aligned_addr(&self, align: usize) -> Self {
        Self::new(**self + align - 1).prev_aligned_addr(align)
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

impl Sub<PhysicalAddress> for PhysicalAddress {
    type Output = usize;

    fn sub(self, rhs: PhysicalAddress) -> usize {
        self.checked_sub(*rhs)
            .expect("Physical address subtraction underflowed")
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, Ord, Eq, PartialOrd, PartialEq)]
pub struct VirtualAddress(RawVirtualAddress);
impl VirtualAddress {
    pub const unsafe fn new_const(addr: RawVirtualAddress) -> Self {
        Self(addr)
    }

    pub const fn raw(&self) -> RawVirtualAddress {
        self.0
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
