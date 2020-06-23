mod bitmap_frame_allocator;
//mod watermark_frame_allocator;

use alloc::boxed::Box;
use alloc::vec::Vec;

use logc::debug;
use multiboot2::{MemoryAreaIter, MemoryAreaType};
use spin::Mutex;

use super::{Frame, PhysicalMemory, PhysicalMemoryKind};
use crate::memory::PhysicalAddress;
use bitmap_frame_allocator::BitmapFrameAllocator;

pub static PHYSICAL_ALLOCATOR: PhysicalMemoryManager = unsafe { PhysicalMemoryManager::new() };

pub trait FrameAllocator {
    fn alloc(&mut self) -> Option<Frame>;
    fn alloc_exact(&mut self, addr: PhysicalAddress) -> Option<Frame>;
    fn free(&mut self, frame: Frame);
    fn has_address(&self, address: PhysicalAddress) -> bool;
    fn has(&self, frame: &Frame) -> bool;
}

pub struct PhysicalMemoryManager {
    areas: Mutex<Vec<Box<dyn FrameAllocator + Send>>>,
}

impl PhysicalMemoryManager {
    /// # Safety
    /// Caller must guarantee that allocations are not made before initialization.
    pub const unsafe fn new() -> Self {
        Self {
            areas: Mutex::new(Vec::new()),
        }
    }

    /// # Safety
    /// Caller must ensure that all addresses passed are valid and correct.
    /// Note that while skipping the first area _may_ prevent collisions with the given bounds,
    /// it is better to provide them just in case.
    pub unsafe fn init(
        &self,
        kernel_start: PhysicalAddress,
        kernel_end: PhysicalAddress,
        multiboot_info_start: PhysicalAddress,
        multiboot_info_end: PhysicalAddress,
        initramfs_start: PhysicalAddress,
        initramfs_end: PhysicalAddress,
        areas: MemoryAreaIter,
    ) {
        let mut bitmaps = self.areas.lock();

        // Skip the probably sub-megabyte first area.
        let areas = areas
            .skip(1)
            .filter(|a| a.typ() == MemoryAreaType::Available);

        for area in areas {
            debug!("Creating BitmapFrameAllocator for area {:x?}", area);
            bitmaps.push(Box::new(BitmapFrameAllocator::new(
                area,
                &[
                    (*kernel_start, *kernel_end),
                    (*multiboot_info_start, *multiboot_info_end),
                    (*initramfs_start, *initramfs_end),
                ],
            )));
        }
    }

    pub fn alloc_frame(&self) -> Option<Frame> {
        let mut areas = self.areas.lock();
        for area in &mut *areas {
            if let s @ Some(_) = area.alloc() {
                return s;
            }
        }
        None
    }

    pub fn alloc_memory(&self, size: usize) -> Option<PhysicalMemory> {
        let n = (size + Frame::SIZE - 1) / Frame::SIZE;
        // NOTE: This should result in the immediate freeing of successfully-allocated frames if there are
        // any failures.
        Some(PhysicalMemory {
            frames: (0..n)
                .map(|_| self.alloc_frame())
                .collect::<Option<Vec<Frame>>>()?,
            kind: PhysicalMemoryKind::Allocated,
        })
    }

    pub fn alloc_memory_exact_contiguous(
        &self,
        address: PhysicalAddress,
        size: usize,
    ) -> Option<PhysicalMemory> {
        todo!()
    }

    pub fn free(&self, frame: Frame) {
        let mut areas = self.areas.lock();
        for area in &mut *areas {
            if area.has(&frame) {
                area.free(frame);
                return;
            }
        }

        panic!("Tried to free an un-allocated frame.");
    }

    pub fn free_memory(&self, memory: PhysicalMemory) {
        for frame in memory.into_frames() {
            self.free(frame)
        }
    }
}
