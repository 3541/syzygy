use multiboot2::{MemoryAreaIter, MemoryArea};

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

impl FrameAllocator for SequentialFrameAllocator
