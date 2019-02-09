mod watermark_frame_allocator;

use crate::KERNEL_BASE;
pub use watermark_frame_allocator::WatermarkFrameAllocator;

const FRAME_ALIGN: usize = 4096;

#[derive(Copy, Clone)]
struct MemoryArea {
    address: usize,
    size: usize,
}

impl MemoryArea {
    fn end_address(&self) -> usize {
        if self.address - KERNEL_BASE + self.size > core::usize::MAX - KERNEL_BASE {
            core::usize::MAX
        } else {
            self.address + self.size
        }
    }
}

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

impl core::ops::Add<usize> for FrameSize {
    type Output = usize;

    fn add(self, other: usize) -> usize {
        self as usize + other
    }
}

impl core::ops::Add<FrameSize> for usize {
    type Output = usize;

    fn add(self, other: FrameSize) -> usize {
        self + other as usize
    }
}

impl core::ops::AddAssign<FrameSize> for usize {
    fn add_assign(&mut self, other: FrameSize) {
        *self = *self + other as usize;
    }
}

#[derive(Debug)]
pub struct Frame {
    address: usize,
    size: FrameSize,
}

impl Frame {
    pub fn address(&self) -> usize {
        self.address
    }

    pub fn end_address(&self) -> usize {
        self.address + self.size as usize
    }
}

pub trait FrameAllocator {
    fn alloc(&mut self, size: FrameSize) -> Option<Frame>;
    fn free(&mut self, frame: Frame);
}

pub fn next_aligned_addr(mut base: usize, align: usize) -> usize {
    if base % align != 0 && base != core::usize::MAX {
        base = (base / align + 1) * align
    }
    base
}
