use multiboot2::MemoryAreaIter;
use spin::{Mutex, MutexGuard};

use super::{
    next_aligned_addr, prev_aligned_addr, Frame, FrameAllocator, PhysicalAddress, FRAME_SIZE,
};

const INITIAL_BITMAP_SIZE: usize = 32768;
// NOTE: temporary static allocation size
pub static mut BITMAP: [usize; INITIAL_BITMAP_SIZE] = [0; INITIAL_BITMAP_SIZE];
pub struct BitmapFrameAllocator {
    bitmap: Mutex<&'static mut [usize]>,
    base: PhysicalAddress,
}

impl BitmapFrameAllocator {
    pub fn new(
        kernel_start: PhysicalAddress,
        kernel_end: PhysicalAddress,
        multiboot_info_start: PhysicalAddress,
        multiboot_info_end: PhysicalAddress,
        areas: MemoryAreaIter,
        bitmap: &'static mut [usize],
    ) -> BitmapFrameAllocator {
        let frame_in_reserved_area = |a: &PhysicalAddress| {
            (*a >= kernel_start && *a <= kernel_end)
                || (*a + FRAME_SIZE <= kernel_end && *a + FRAME_SIZE >= kernel_start)
                || (*a <= kernel_start && *a + FRAME_SIZE >= kernel_end)
                || (*a >= multiboot_info_start && *a <= multiboot_info_end)
                || (*a + FRAME_SIZE <= multiboot_info_end
                    && *a + FRAME_SIZE >= multiboot_info_start)
                || (*a <= multiboot_info_start && *a + FRAME_SIZE >= multiboot_info_end)
        };
        let mut ret = BitmapFrameAllocator {
            bitmap: Mutex::new(bitmap),
            base: next_aligned_addr(
                areas
                    .clone()
                    .filter(|a| a.size() as usize >= FRAME_SIZE)
                    .min_by_key(|a| a.start_address())
                    .expect("No available areas to initialize")
                    .start_address() as usize,
                FRAME_SIZE,
            ),
        };
        {
            let mut lock = ret.bitmap.lock();
            for area in areas.filter(|a| a.size() as usize >= FRAME_SIZE) {
                let start_address = next_aligned_addr(area.start_address() as usize, FRAME_SIZE);
                let end_address = prev_aligned_addr(area.end_address() as usize, FRAME_SIZE);
                for frame_start in (start_address..=end_address)
                    .step_by(FRAME_SIZE)
                    .filter(frame_in_reserved_area)
                {
                    if ret.field(frame_start) >= INITIAL_BITMAP_SIZE {
                        error!("Index too large at frame 0x{:x}", frame_start);
                    }

                    ret.set_used(frame_start, &mut lock);
                }
            }

            // FIXME hack -- set used the original PML4 so it can be freed when we remap the kernel_end
            ret.set_used(0x125000, &mut lock);
        }

        ret
    }

    fn field(&self, address: PhysicalAddress) -> usize {
        (address - self.base) / FRAME_SIZE / core::mem::size_of::<usize>() / 8
    }

    fn mask(address: PhysicalAddress) -> usize {
        1 << (address % core::mem::size_of::<usize>())
    }

    fn address(&self, index: usize, bit_index: usize) -> PhysicalAddress {
        index * 8 * core::mem::size_of::<usize>() * FRAME_SIZE + self.base + bit_index * FRAME_SIZE
    }

    // FIXME: This is weird. self is immutable because there is an immutable reference to the lock.
    // Does this actually make sense? Who knows...
    fn set_used(&self, address: PhysicalAddress, lock: &mut MutexGuard<&'static mut [usize]>) {
        let field = self.field(address);
        let mask = Self::mask(address);

        if lock[field] & mask == 1 {
            panic!("Tried to set an already used address as used.");
        }

        lock[field] |= mask;
    }
}

impl FrameAllocator for BitmapFrameAllocator {
    fn alloc(&mut self) -> Option<Frame> {
        fn first_unset_bit(field: usize) -> usize {
            let ret: usize;
            unsafe {
                asm!("tzcnt $1, $0" : "=r"(ret) : "r"(!field));
            }
            ret
        }

        for (i, field) in (**self.bitmap.lock()).iter_mut().enumerate() {
            if *field == 0 {
                let unset = first_unset_bit(*field);
                let r_addr = self.address(i, unset);
                *field |= 1 << unset;
                trace!(
                    "Allocated 0x{:x} from index {} bit {}",
                    self.address(i, unset),
                    i,
                    unset
                );
                return Some(Frame(self.address(i, unset)));
            } else if *field == core::usize::MAX {
                trace!("Full field");
                continue;
            } else {
                let unset = first_unset_bit(*field);
                let r_addr = self.address(i, unset);
                *field |= 1 << unset;
                trace!(
                    "Allocated 0x{:x} from index {} bit {}",
                    self.address(i, unset),
                    i,
                    unset
                );
                return Some(Frame(self.address(i, unset)));
            }
        }
        None
    }

    fn free(&mut self, frame: Frame) {
        trace!("Freeing {:x?}", frame);

        let field = self.field(frame.address());
        let mask = Self::mask(frame.address());
        let mut bitmap = self.bitmap.lock();
        if bitmap[field] & mask != 1 {
            panic!("DOUBLE FREE");
        }

        bitmap[field] ^= mask;
    }
}
