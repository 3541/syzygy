use multiboot2::{MemoryArea, MemoryAreaIter};

use crate::memory::{
    next_aligned_addr, Frame, FrameAllocator, FrameSize, /*MemoryArea,*/ FRAME_ALIGN,
};
use crate::KERNEL_BASE;
pub struct WatermarkFrameAllocator {
    next_frame: usize,
    area: Option<&'static MemoryArea>,
    areas: MemoryAreaIter<'static>,
    kernel_start: usize,
    kernel_end: usize,
    multiboot_info_start: usize,
    multiboot_info_end: usize,
}

impl WatermarkFrameAllocator {
    fn next_area(&mut self, min_size: usize) {
        self.area = self
            .areas
            .clone()
            .filter(|a| a.end_address() as usize >= self.next_frame + min_size)
            .min_by_key(|a| a.start_address());

        if let Some(area) = self.area {
            self.next_frame = core::cmp::max(
                self.next_frame,
                next_aligned_addr(area.start_address() as usize, FRAME_ALIGN),
            );
        }
    }

    pub fn new(
        kernel_start: usize,
        kernel_end: usize,
        multiboot_info_start: usize,
        multiboot_info_end: usize,
        areas: MemoryAreaIter<'static>,
    ) -> Self {
        let mut alloc = WatermarkFrameAllocator {
            kernel_start: kernel_start - KERNEL_BASE,
            kernel_end: kernel_end - KERNEL_BASE,
            multiboot_info_start: multiboot_info_start - KERNEL_BASE,
            multiboot_info_end: multiboot_info_end - KERNEL_BASE,
            areas,
            area: None,
            next_frame: 0,
        };
        alloc.next_area(FrameSize::Small as usize);
        alloc
    }
}

// Physical frame allocator
impl FrameAllocator for WatermarkFrameAllocator {
    fn alloc(&mut self, size: FrameSize) -> Option<Frame> {
        if let Some(area) = self.area {
            let frame = Frame {
                address: self.next_frame,
                size,
            };

            if frame.end_address() > area.end_address() as usize {
                self.next_area(frame.size as usize);
            } else if (frame.address() >= self.kernel_start && frame.address() <= self.kernel_end)
                || (frame.end_address() <= self.kernel_end
                    && frame.end_address() >= self.kernel_start)
                || (frame.address() <= self.kernel_start && frame.end_address() >= self.kernel_end)
            {
                self.next_frame = next_aligned_addr(self.kernel_end + 1, FRAME_ALIGN);
            } else if (frame.address() > self.multiboot_info_start
                && frame.address() < self.multiboot_info_end)
                || (frame.end_address() < self.multiboot_info_end
                    && frame.end_address() > self.multiboot_info_start)
                || (frame.address() <= self.multiboot_info_start
                    && frame.end_address() >= self.multiboot_info_end)
            {
                self.next_frame = next_aligned_addr(self.multiboot_info_end + 1, FRAME_ALIGN);
            } else {
                self.next_frame += frame.size;
                return Some(frame);
            }
            self.alloc(size)
        } else {
            None
        }
    }

    fn free(&mut self, _frame: Frame) {
        unimplemented!()
    }
}
