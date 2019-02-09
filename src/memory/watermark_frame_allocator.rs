use multiboot2::MemoryAreaIter;

use crate::memory::{next_aligned_addr, Frame, FrameAllocator, FrameSize, MemoryArea, FRAME_ALIGN};
pub struct WatermarkFrameAllocator {
    next_frame: usize,
    area: Option<MemoryArea>,
    areas: MemoryAreaIter,
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
            .map(|a| MemoryArea {
                address: a.start_address() as usize + crate::KERNEL_BASE,
                size: a.size() as usize,
            })
            .filter(|a| {
                let addr = next_aligned_addr(a.end_address(), FRAME_ALIGN);
                addr >= self.next_frame && a.size >= min_size && addr != core::usize::MAX
            })
            .min_by_key(|a| {
                crate::println!("0x{:x}", self.next_frame);
                a.address
            });

        if let Some(area) = self.area {
            let first_addr = next_aligned_addr(area.address, FRAME_ALIGN);
            self.next_frame = core::cmp::max(self.next_frame, first_addr);
        }
    }

    pub fn new(
        kernel_start: usize,
        kernel_end: usize,
        multiboot_info_start: usize,
        multiboot_info_end: usize,
        areas: MemoryAreaIter,
    ) -> Self {
        let mut alloc = WatermarkFrameAllocator {
            kernel_start,
            kernel_end,
            multiboot_info_start,
            multiboot_info_end,
            areas,
            area: None,
            next_frame: crate::KERNEL_BASE,
        };
        alloc.next_area(FrameSize::Small as usize);
        alloc
    }
}

impl FrameAllocator for WatermarkFrameAllocator {
    fn alloc(&mut self, size: FrameSize) -> Option<Frame> {
        if let Some(area) = self.area {
            let frame = Frame {
                address: self.next_frame,
                size,
            };

            if frame.address + frame.size > area.end_address() as usize {
                self.next_area(frame.size as usize);
            } else if (frame.address > self.kernel_start && frame.address < self.kernel_end)
                || (frame.address + frame.size < self.kernel_end
                    && frame.address + frame.size > self.kernel_start)
            {
                self.next_frame = next_aligned_addr(self.kernel_end + 1, FRAME_ALIGN);
            } else if (frame.address > self.multiboot_info_start
                && frame.address < self.multiboot_info_end)
                || (frame.address + frame.size < self.multiboot_info_end
                    && frame.address + frame.size > self.multiboot_info_start)
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
