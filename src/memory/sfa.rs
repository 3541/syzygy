use multiboot2::{MemoryArea, MemoryAreaIter};

use memory::{Frame, FrameAllocator};

pub struct SequentialFrameAllocator {
    next_free: Frame,
    current_area: Option<&'static MemoryArea>,
    areas: MemoryAreaIter,
    kernel_start: Frame,
    kernel_end: Frame,
    multiboot_start: Frame,
    multiboot_end: Frame,
}

impl FrameAllocator for SequentialFrameAllocator {
    fn alloc_frame(&mut self) -> Option<Frame> {
        // Space left?
        if let Some(area) = self.current_area {
            // Final frame of the current area
            let current_final_frame = Frame::containing_address(area.end_address());
            if self.next_free > current_final_frame {
                self.advance_area();
            } else if self.next_free >= self.kernel_start && self.next_free <= self.kernel_end {
                self.next_free = Frame {
                    number: self.kernel_end.number + 1,
                };
            } else if self.next_free >= self.multiboot_start && self.next_free <= self.multiboot_end
            {
                self.next_free = Frame {
                    number: self.multiboot_end.number + 1,
                };
            } else {
                self.next_free.number += 1;
                return Some(Frame {
                    number: self.next_free.number - 1,
                });
            }

            self.alloc_frame()
        } else {
            None
        }
    }

    fn free_frame(&mut self, frame: Frame) {
        unimplemented!();
    }
}

impl SequentialFrameAllocator {
    pub fn new(
        kernel_start: usize,
        kernel_end: usize,
        multiboot_start: usize,
        multiboot_end: usize,
        memory_areas: MemoryAreaIter,
    ) -> SequentialFrameAllocator {
        let mut ret = SequentialFrameAllocator {
            next_free: Frame::containing_address(0),
            current_area: None,
            areas: memory_areas,
            kernel_start: Frame::containing_address(kernel_start),
            kernel_end: Frame::containing_address(kernel_end),
            multiboot_start: Frame::containing_address(multiboot_start),
            multiboot_end: Frame::containing_address(multiboot_end),
        };
        ret.advance_area();
        ret
    }

    fn advance_area(&mut self) {
        self.current_area = self
            .areas
            .clone()
            .filter(|area| Frame::containing_address(area.end_address()) >= self.next_free)
            .min_by_key(|area| area.start_address());

        if let Some(area) = self.current_area {
            let start_frame = Frame::containing_address(area.start_address());
            if self.next_free < start_frame {
                self.next_free = start_frame;
            }
        }
    }
}
