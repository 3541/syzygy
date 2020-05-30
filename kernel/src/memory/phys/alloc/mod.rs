mod bitmap_frame_allocator;
//mod watermark_frame_allocator;

use logc::trace;
use multiboot2::MemoryAreaIter;
use spin::{Mutex, MutexGuard};

use super::Frame;
use crate::memory::PhysicalAddress;
use bitmap_frame_allocator::BitmapFrameAllocator;

// NOTE: temporary static allocation size
const INITIAL_BITMAP_SIZE: usize = 32768;

pub static FRAME_ALLOCATOR: GlobalFrameAllocator = unsafe { GlobalFrameAllocator::new() };
pub static mut BITMAP: [usize; INITIAL_BITMAP_SIZE] = [0; INITIAL_BITMAP_SIZE];

pub trait FrameAllocator {
    fn alloc(&mut self) -> Option<Frame>;
    fn free(&mut self, frame: Frame);
}

pub struct GlobalFrameAllocator<'a>(Mutex<BitmapFrameAllocator<'a>>);

impl<'a> GlobalFrameAllocator<'a> {
    pub const unsafe fn new() -> Self {
        Self(Mutex::new(BitmapFrameAllocator::empty()))
    }

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
        self.0.lock().init(
            kernel_start,
            kernel_end,
            multiboot_info_start,
            multiboot_info_end,
            initramfs_start,
            initramfs_end,
            areas,
            &mut BITMAP,
        );
    }

    pub fn lock(&self) -> MutexGuard<BitmapFrameAllocator<'a>> {
        trace!("Trying to get FRAME_ALLOCATOR lock");
        let ret = self.0.lock();
        if ret.is_uninitialized() {
            panic!("Attempted to lock frame allocator before initializing");
        }
        trace!("Got FRAME_ALLOCATOR lock");
        ret
    }
}
