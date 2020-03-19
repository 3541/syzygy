use core::convert::Into;
use core::fmt;
use core::marker::Sized;
use core::ops::{Add, AddAssign, Deref, Sub};

mod alloc;
pub mod bitmap_frame_allocator;
mod heap;
pub mod paging;
//mod watermark_frame_allocator;

pub use bitmap_frame_allocator::BitmapFrameAllocator;
pub use heap::init_heap;
//pub use watermark_frame_allocator::WatermarkFrameAllocator;

const FRAME_SIZE: usize = 4096;
const PAGE_SIZE: usize = FRAME_SIZE;

const SIGN_EX_INVALID_BASE: usize = 0x0000_8000_0000_0000;
const SIGN_EX_INVALID_TOP: usize = 0xFFFF_8000_0000_0000;

pub type RawPhysicalAddress = usize;
pub type RawVirtualAddress = usize;

pub trait Address: Deref<Target = usize> + Sized {
    fn new(addr: usize) -> Self {
        assert!(
            addr < SIGN_EX_INVALID_BASE || addr >= SIGN_EX_INVALID_TOP,
            "Invalid address! Sign extension set incorrectly."
        );
        unsafe { Self::new_unchecked(addr) }
    }

    unsafe fn new_unchecked(addr: usize) -> Self;

    fn is_aligned(&self, align: usize) -> bool {
        **self % align == 0
    }

    fn prev_aligned_addr(&self, align: usize) -> Self {
        Self::new(**self & !(align - 1))
    }

    fn next_aligned_addr(&self, align: usize) -> Self {
        Self::new(**self + align - 1).prev_aligned_addr(align)
    }
}

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

impl Into<VirtualAddress> for PhysicalAddress {
    fn into(self) -> VirtualAddress {
        VirtualAddress::new(*self)
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

#[derive(Debug, Copy, Clone)]
pub struct Frame(PhysicalAddress);

impl Frame {
    pub fn address(&self) -> PhysicalAddress {
        self.0
    }

    pub fn end_address(&self) -> PhysicalAddress {
        self.0 + FRAME_SIZE
    }

    pub fn containing_address(address: PhysicalAddress) -> Frame {
        Frame(address.prev_aligned_addr(FRAME_SIZE))
    }

    fn range_inclusive(from: Frame, to: Frame) -> FrameIterator {
        FrameIterator { from, to }
    }
}

struct FrameIterator {
    from: Frame,
    to: Frame,
}

impl Iterator for FrameIterator {
    type Item = Frame;

    fn next(&mut self) -> Option<Frame> {
        if self.from.address() <= self.to.address() {
            let frame = self.from.clone();
            self.from.0 += FRAME_SIZE;
            Some(frame)
        } else {
            None
        }
    }
}

pub trait FrameAllocator {
    fn alloc(&mut self) -> Option<Frame>;
    fn free(&mut self, frame: Frame);
}
