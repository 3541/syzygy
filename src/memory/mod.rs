mod sfa;

const PAGE_SIZE: usize = 4096;

pub trait FrameAllocator {
    fn alloc_frame(&mut self) -> Option<Frame>;
    fn free_frame(&mut self, frame: Frame);
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Frame {
    number: usize,
}

impl Frame {
    fn containing_address(addr: usize) -> Frame {
        Frame { number: addr / PAGE_SIZE }
    }
}
