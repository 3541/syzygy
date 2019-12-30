pub mod paging;
mod watermark_frame_allocator;

use core::ops::{Add, AddAssign};

use paging::PhysicalAddress;
pub use watermark_frame_allocator::WatermarkFrameAllocator;

const FRAME_ALIGN: usize = 4096;

#[cfg(target_arch = "x86_64")]
#[derive(Debug, Copy, Clone)]
pub enum FrameSize {
    Small = 0x1000,     // 4K
    Large = 0x20_0000,  // 2M
    Huge = 0x4000_0000, // 1G
}

#[cfg(target_arch = "x86")]
#[derive(Debug, Copy, Clone)]
pub enum FrameSize {
    Small = 0x1000,    // 4K
    Large = 0x40_0000, // 4M
}

impl Add<usize> for FrameSize {
    type Output = usize;

    fn add(self, other: usize) -> usize {
        self as usize + other
    }
}

impl Add<FrameSize> for usize {
    type Output = usize;

    fn add(self, other: FrameSize) -> usize {
        self + other as usize
    }
}

impl AddAssign<FrameSize> for usize {
    fn add_assign(&mut self, other: FrameSize) {
        *self = *self + other as usize;
    }
}

#[derive(Debug)]
pub struct Frame {
    address: PhysicalAddress,
    size: FrameSize,
}

impl Frame {
    pub fn address(&self) -> PhysicalAddress {
        self.address
    }

    pub fn end_address(&self) -> PhysicalAddress {
        self.address + self.size as usize
    }
}

pub trait FrameAllocator {
    fn alloc(&mut self, size: FrameSize) -> Option<Frame>;
    fn free(&mut self, frame: Frame);
}

pub fn next_aligned_addr(base: PhysicalAddress, align: usize) -> PhysicalAddress {
    base - base % align
}
