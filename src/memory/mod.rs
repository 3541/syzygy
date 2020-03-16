pub mod bitmap_frame_allocator;
mod fake;
pub mod paging;
mod watermark_frame_allocator;

pub use bitmap_frame_allocator::BitmapFrameAllocator;
pub use watermark_frame_allocator::WatermarkFrameAllocator;

const FRAME_SIZE: usize = 4096;

#[global_allocator]
static ALLOCATOR: fake::FakeAllocator = fake::FakeAllocator;

pub type PhysicalAddress = usize;
pub type VirtualAddress = usize;

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
        Frame(prev_aligned_addr(address, FRAME_SIZE))
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

pub fn prev_aligned_addr(base: PhysicalAddress, align: usize) -> PhysicalAddress {
    base - base % align
}

pub fn next_aligned_addr(base: PhysicalAddress, align: usize) -> PhysicalAddress {
    prev_aligned_addr(base, align) + align
}
