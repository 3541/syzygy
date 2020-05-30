use core::mem::MaybeUninit;

use logc::{error, trace};
use multiboot2::MemoryAreaIter;
use spin::MutexGuard;

use super::FrameAllocator;
use crate::memory::{Address, Frame, PhysicalAddress};

pub struct BitmapFrameAllocator<'a> {
    bitmap: MaybeUninit<&'a mut [usize]>,
    base: PhysicalAddress,
}

// EVERYTHING in here is only safe if called through GlobalFrameAllocator.lock
impl<'a> BitmapFrameAllocator<'a> {
    pub const unsafe fn empty() -> Self {
        Self {
            bitmap: MaybeUninit::uninit(),
            base: PhysicalAddress::new_const(PhysicalAddress::SIGN_EX_INVALID_BASE + 0x1000),
        }
    }

    pub unsafe fn init(
        &mut self,
        kernel_start: PhysicalAddress,
        kernel_end: PhysicalAddress,
        multiboot_info_start: PhysicalAddress,
        multiboot_info_end: PhysicalAddress,
        initramfs_start: PhysicalAddress,
        initramfs_end: PhysicalAddress,
        areas: MemoryAreaIter,
        bitmap: &'static mut [usize],
    ) {
        let frame_in_reserved_area = |a: &PhysicalAddress| {
            (kernel_start <= *a && *a <= kernel_end)
                || (kernel_start <= *a + Frame::SIZE && *a + Frame::SIZE <= kernel_end)
                || (*a <= kernel_start && kernel_end <= *a + Frame::SIZE)
                || (multiboot_info_start <= *a && *a <= multiboot_info_end)
                || (multiboot_info_start <= *a + Frame::SIZE
                    && *a + Frame::SIZE <= multiboot_info_end)
                || (*a <= multiboot_info_start && multiboot_info_end <= *a + Frame::SIZE)
                || (initramfs_start <= *a && *a <= initramfs_end)
                || (initramfs_start <= *a + Frame::SIZE && *a + Frame::SIZE <= initramfs_end)
                || (*a <= initramfs_start && initramfs_end <= *a + Frame::SIZE)
        };
        self.base = PhysicalAddress::new(
            areas
                .clone()
                .filter(|a| a.size() as usize >= Frame::SIZE)
                .min_by_key(|a| a.start_address())
                .expect("No available areas to initialize")
                .start_address() as usize,
        )
        .next_aligned_addr(Frame::SIZE);
        self.bitmap.write(bitmap);

        let mut last_area_end = PhysicalAddress::new(0);

        for area in areas.filter(|a| a.size() as usize >= Frame::SIZE) {
            let start_address =
                PhysicalAddress::new(area.start_address() as usize).next_aligned_addr(Frame::SIZE);
            let mut range_start = start_address;
            let end_address =
                PhysicalAddress::new(area.end_address() as usize).prev_aligned_addr(Frame::SIZE);
            if last_area_end < start_address {
                range_start = last_area_end;
            }

            for frame_start in (*range_start..=*end_address)
                .step_by(Frame::SIZE)
                .map(PhysicalAddress::new)
                .filter(|a| frame_in_reserved_area(a) || (range_start <= *a && *a < start_address))
            {
                if self.field(frame_start) >= self.bitmap().len() {
                    error!("Index too large at frame {}", frame_start);
                }
                self.set_used(frame_start);
            }

            last_area_end = end_address;
        }
    }

    unsafe fn bitmap(&mut self) -> &mut [usize] {
        *(self.bitmap.as_mut_ptr())
    }

    pub fn is_uninitialized(&self) -> bool {
        !self.base.is_valid()
    }

    fn field(&self, address: PhysicalAddress) -> usize {
        (address - self.base) / Frame::SIZE / core::mem::size_of::<usize>() / 8
    }

    fn mask(address: PhysicalAddress) -> usize {
        1 << (*address / Frame::SIZE % (core::mem::size_of::<usize>() * 8))
    }

    fn address(&self, index: usize, bit_index: usize) -> PhysicalAddress {
        self.base
            + index * 8 * core::mem::size_of::<usize>() * Frame::SIZE
            + bit_index * Frame::SIZE
    }

    unsafe fn set_used(&mut self, address: PhysicalAddress) {
        let field = self.field(address);
        let mask = Self::mask(address);

        //        trace!("Setting {} used at {} with 0x{:x}", address, field, mask);

        if self.bitmap()[field] & mask != 0 {
            panic!("Tried to set an already used address {} as used.", address);
        }

        self.bitmap()[field] |= mask;

        assert!(self.bitmap()[field] & mask != 0, "Failed to set.");
    }
}

// The trait is implemented on MutexGuard<BitmapFrameAllocator> to ensure that a
// valid lock is held, and therefore that it has been initialized.
impl FrameAllocator for MutexGuard<'_, BitmapFrameAllocator<'_>> {
    fn alloc(&mut self) -> Option<Frame> {
        fn first_unset_bit(field: usize) -> usize {
            let ret: usize;
            unsafe {
                llvm_asm!("tzcnt $1, $0" : "=r"(ret) : "r"(!field));
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
        let mask = BitmapFrameAllocator::mask(frame.address());

        trace!("Freeing {:x?} with {} 0x{:x}", frame, field, mask);

        unsafe {
            if self.bitmap()[field] & mask == 0 {
                panic!("DOUBLE FREE");
            }

            self.bitmap()[field] ^= mask;
        }
    }
}
