use multiboot2::{MemoryArea, MemoryAreaIter};

use crate::constants::KERNEL_BASE;
use crate::memory::{next_aligned_addr, Frame, FrameAllocator, PhysicalAddress, FRAME_SIZE};

pub struct WatermarkFrameAllocator<'a> {
    next_frame: PhysicalAddress,
    area: Option<&'a MemoryArea>,
    areas: MemoryAreaIter<'a>,
    kernel_start: PhysicalAddress,
    kernel_end: PhysicalAddress,
    multiboot_info_start: PhysicalAddress,
    multiboot_info_end: PhysicalAddress,
}

impl<'a> WatermarkFrameAllocator<'a> {
    fn next_area(&mut self) {
        self.area = self
            .areas
            .clone()
            .filter(|a| a.end_address() as usize >= self.next_frame + FRAME_SIZE)
            .min_by_key(|a| a.start_address());

        if let Some(area) = self.area {
            self.next_frame = core::cmp::max(
                self.next_frame,
                next_aligned_addr(area.start_address() as usize, FRAME_SIZE),
            );
        }
    }

    pub fn new(
        kernel_start: usize,
        kernel_end: usize,
        multiboot_info_start: usize,
        multiboot_info_end: usize,
        areas: MemoryAreaIter<'a>,
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
        alloc.next_area();
        alloc
    }
}

// Physical frame allocator
impl FrameAllocator for WatermarkFrameAllocator<'_> {
    fn alloc(&mut self) -> Option<Frame> {
        trace!("ENTERED WatermarkFrameAllocator::alloc");
        if let Some(area) = self.area {
            let frame = Frame(self.next_frame);
            trace!("Allocating frame {:#x?}", frame);

            if frame.end_address() > area.end_address() as usize {
                self.next_area();
            } else if (frame.address() >= self.kernel_start && frame.address() <= self.kernel_end)
                || (frame.end_address() <= self.kernel_end
                    && frame.end_address() >= self.kernel_start)
                || (frame.address() <= self.kernel_start && frame.end_address() >= self.kernel_end)
            {
                self.next_frame = next_aligned_addr(self.kernel_end + 1, FRAME_SIZE);
            } else if (frame.address() > self.multiboot_info_start
                && frame.address() < self.multiboot_info_end)
                || (frame.end_address() < self.multiboot_info_end
                    && frame.end_address() > self.multiboot_info_start)
                || (frame.address() <= self.multiboot_info_start
                    && frame.end_address() >= self.multiboot_info_end)
            {
                self.next_frame = next_aligned_addr(self.multiboot_info_end + 1, FRAME_SIZE);
            } else {
                self.next_frame += FRAME_SIZE;
                return Some(frame);
            }
            self.alloc()
        } else {
            None
        }
    }

    fn free(&mut self, frame: Frame) {
        warn!(
            "WatermarkFrameAllocator cannot free. Tried to free {:x?}",
            frame
        )
    }
}
