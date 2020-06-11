mod bitmap_frame_allocator;
//mod watermark_frame_allocator;

use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;

use logc::debug;
use multiboot2::{MemoryAreaIter, MemoryAreaType};
use spin::{Mutex, MutexGuard};

use super::Frame;
use crate::memory::size::KB;
use crate::memory::PhysicalAddress;
use bitmap_frame_allocator::BitmapFrameAllocator;

// NOTE: temporary static allocation size
const INITIAL_BITMAP_SIZE: usize = 32768;

pub static FRAME_ALLOCATOR: PhysicalMemoryManager = unsafe { PhysicalMemoryManager::new() };
pub static mut BITMAP: [usize; INITIAL_BITMAP_SIZE] = [0; INITIAL_BITMAP_SIZE];

pub trait FrameAllocator {
    fn alloc(&mut self) -> Option<Frame>;
    fn free(&mut self, frame: Frame);
    fn has(&self, frame: Frame) -> bool;
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

    pub fn alloc(&self) -> Option<Frame> {
        let mut areas = self.areas.lock();
        for area in &mut *areas {
            if let s @ Some(_) = area.alloc() {
                return s;
            }
        }
        None
    }

    pub fn free(&self, frame: Frame) {
        let mut areas = self.areas.lock();
        for area in &mut *areas {
            if area.has(frame) {
                area.free(frame);
                return;
            }
        }

        panic!("Tried to free an un-allocated frame.");
    }
}
