use multiboot2::MemoryAreaIter;
use spin::Mutex;

use super::{
    next_aligned_addr, prev_aligned_addr, Frame, FrameAllocator, PhysicalAddress, FRAME_SIZE,
};

const INITIAL_BITMAP_SIZE: usize = 32768;
// NOTE: temporary static allocation size
pub static mut BITMAP: [usize; INITIAL_BITMAP_SIZE] = [1; INITIAL_BITMAP_SIZE];
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
        let frame_outside_reserved_area = |a: &PhysicalAddress| {
            !((*a >= kernel_start && *a <= kernel_end)
                || (*a + FRAME_SIZE <= kernel_end && *a + FRAME_SIZE >= kernel_start)
                || (*a <= kernel_start && *a + FRAME_SIZE >= kernel_end)
                || (*a >= multiboot_info_start && *a <= multiboot_info_end)
                || (*a + FRAME_SIZE <= multiboot_info_end
                    && *a + FRAME_SIZE >= multiboot_info_start)
                || (*a <= multiboot_info_start && *a + FRAME_SIZE >= multiboot_info_end))
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
        for area in areas.filter(|a| a.size() as usize >= FRAME_SIZE) {
            let start_address = next_aligned_addr(area.start_address() as usize, FRAME_SIZE);
            let end_address = prev_aligned_addr(area.end_address() as usize, FRAME_SIZE);
            for frame_start in (start_address..=end_address)
                .step_by(FRAME_SIZE)
                .filter(frame_outside_reserved_area)
            {
                if ret.field(frame_start) >= INITIAL_BITMAP_SIZE {
                    error!("Index too large at frame 0x{:x}", frame_start);
                }
                // TODO: this is fucked
                ret.bitmap.lock()[ret.field(frame_start)] ^=
                    BitmapFrameAllocator::mask(frame_start);
            }
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
}

impl Index<PhysicalAddress> for BitmapFrameAllocator {
    type Output = FrameState;

    fn index(&self, address: PhysicalAddress) -> &Self::Output {
        let f = self.bitmap.lock()[self.field(address)];
        if f & BitmapFrameAllocator::mask(address) == FrameState::FREE as usize {
            &FrameState::FREE
        } else {
            &FrameState::USED
        }
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
            if *field == FrameState::FREE as usize {
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

    fn free(&mut self, _frame: Frame) {
        warn!("Freeing not yet implemented!");
    }
}
