use core::mem::MaybeUninit;

use logc::{debug, error, trace};
use multiboot2::MemoryAreaIter;

use super::FrameAllocator;
use crate::memory::{Address, Frame, PhysicalAddress, FRAME_SIZE};

pub struct BitmapFrameAllocator {
    bitmap: MaybeUninit<&'static mut [usize]>,
    base: PhysicalAddress,
}

// EVERYTHING in here is only safe if called through GlobalFrameAllocator.lock
impl BitmapFrameAllocator {
    pub const unsafe fn empty() -> Self {
        Self {
            bitmap: MaybeUninit::uninit(),
            base: PhysicalAddress::new_const(crate::memory::SIGN_EX_INVALID_BASE + 0x1000),
        }
    }

    pub unsafe fn init(
        &mut self,
        kernel_start: PhysicalAddress,
        kernel_end: PhysicalAddress,
        multiboot_info_start: PhysicalAddress,
        multiboot_info_end: PhysicalAddress,
        areas: MemoryAreaIter,
        bitmap: &'static mut [usize],
    ) {
        let frame_in_reserved_area = |a: &PhysicalAddress| {
            (*a >= kernel_start && *a <= kernel_end)
                || (*a + FRAME_SIZE <= kernel_end && *a + FRAME_SIZE >= kernel_start)
                || (*a <= kernel_start && *a + FRAME_SIZE >= kernel_end)
                || (*a >= multiboot_info_start && *a <= multiboot_info_end)
                || (*a + FRAME_SIZE <= multiboot_info_end
                    && *a + FRAME_SIZE >= multiboot_info_start)
                || (*a <= multiboot_info_start && *a + FRAME_SIZE >= multiboot_info_end)
        };
        self.base = PhysicalAddress::new(
            areas
                .clone()
                .filter(|a| a.size() as usize >= FRAME_SIZE)
                .min_by_key(|a| a.start_address())
                .expect("No available areas to initialize")
                .start_address() as usize,
        )
        .next_aligned_addr(FRAME_SIZE);
        self.bitmap.write(bitmap);

        for area in areas.filter(|a| a.size() as usize >= FRAME_SIZE) {
            let start_address =
                PhysicalAddress::new(area.start_address() as usize).next_aligned_addr(FRAME_SIZE);
            let end_address =
                PhysicalAddress::new(area.end_address() as usize).prev_aligned_addr(FRAME_SIZE);
            for frame_start in (*start_address..=*end_address)
                .step_by(FRAME_SIZE)
                .map(|a| PhysicalAddress::new(a))
                .filter(frame_in_reserved_area)
            {
                if self.field(frame_start) >= self.bitmap().len() {
                    error!("Index too large at frame {}", frame_start);
                }
                self.set_used(frame_start);
            }
        }
    }

    unsafe fn bitmap(&mut self) -> &mut [usize] {
        *(self.bitmap.as_mut_ptr())
    }

    pub fn is_uninitialized(&self) -> bool {
        !self.base.is_valid()
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

    unsafe fn set_used(&mut self, address: PhysicalAddress) {
        let field = self.field(address);
        let mask = Self::mask(address);

        trace!("Setting {} used at {} with 0x{:x}", address, field, mask);

        if self.bitmap()[field] & mask != 0 {
            panic!("Tried to set an already used address {} as used.", address);
        }

        self.bitmap()[field] |= mask;

        assert!(self.bitmap()[field] & mask != 0, "Failed to set.");
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

        for (i, field) in unsafe { self.bitmap() }.iter_mut().enumerate() {
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

        unsafe {
            if self.bitmap()[field] & mask == 0 {
                panic!("DOUBLE FREE");
            }

            self.bitmap()[field] ^= mask;
        }
    }
}
