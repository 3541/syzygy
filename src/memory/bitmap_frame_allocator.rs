use logc::{error, trace};
use multiboot2::MemoryAreaIter;
use spin::{Mutex, MutexGuard};

use super::{Address, Frame, FrameAllocator, PhysicalAddress, FRAME_SIZE};

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
        let ret = BitmapFrameAllocator {
            bitmap: Mutex::new(bitmap),
            base: PhysicalAddress::new(
                areas
                    .clone()
                    .filter(|a| a.size() as usize >= FRAME_SIZE)
                    .min_by_key(|a| a.start_address())
                    .expect("No available areas to initialize")
                    .start_address() as usize,
            )
            .next_aligned_addr(FRAME_SIZE),
        };
        {
            let mut lock = ret.bitmap.lock();
            for area in areas.filter(|a| a.size() as usize >= FRAME_SIZE) {
                let start_address = PhysicalAddress::new(area.start_address() as usize)
                    .next_aligned_addr(FRAME_SIZE);
                let end_address =
                    PhysicalAddress::new(area.end_address() as usize).prev_aligned_addr(FRAME_SIZE);
                for frame_start in (*start_address..=*end_address)
                    .step_by(FRAME_SIZE)
                    .map(|a| PhysicalAddress::new(a))
                    .filter(frame_in_reserved_area)
                {
                    if ret.field(frame_start) >= INITIAL_BITMAP_SIZE {
                        error!("Index too large at frame {}", frame_start);
                    }

                    ret.set_used(frame_start, &mut lock);
                }
            }
        }

        ret
    }

    fn field(&self, address: PhysicalAddress) -> usize {
        (address - self.base) / FRAME_SIZE / core::mem::size_of::<usize>() / 8
    }

    fn mask(address: PhysicalAddress) -> usize {
        1 << (*address / FRAME_SIZE % (core::mem::size_of::<usize>() * 8))
    }

    fn address(&self, index: usize, bit_index: usize) -> PhysicalAddress {
        self.base + index * 8 * core::mem::size_of::<usize>() * FRAME_SIZE + bit_index * FRAME_SIZE
    }

    // FIXME: This is weird. self is immutable because there is an immutable reference to the lock.
    // Does this actually make sense? Who knows...
    fn set_used(&self, address: PhysicalAddress, lock: &mut MutexGuard<&'static mut [usize]>) {
        let field = self.field(address);
        let mask = Self::mask(address);

        trace!("Setting {} used at {} with 0x{:x}", address, field, mask);

        if lock[field] & mask != 0 {
            panic!("Tried to set an already used address {} as used.", address);
        }

        lock[field] |= mask;

        assert!(lock[field] & mask != 0, "Failed to set.");
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

        trace!("Allocating...");

        for (i, field) in (**self.bitmap.lock()).iter_mut().enumerate() {
            let bit = if *field == 0 {
                trace!("Empty field.");
                0
            } else if *field == core::usize::MAX {
                trace!("Full field");
                continue;
            } else {
                first_unset_bit(*field)
            };

            *field |= 1 << bit;
            let address = self.address(i, bit);
            trace!("Allocated {} from index {} bit {}", address, i, bit);
            return Some(Frame(address));
        }
        None
    }

    fn free(&mut self, frame: Frame) {
        let field = self.field(frame.address());
        let mask = Self::mask(frame.address());

        trace!("Freeing {:x?} with {} 0x{:x}", frame, field, mask);

        let mut bitmap = self.bitmap.lock();
        if bitmap[field] & mask == 0 {
            panic!("DOUBLE FREE");
        }

        bitmap[field] ^= mask;
    }
}
