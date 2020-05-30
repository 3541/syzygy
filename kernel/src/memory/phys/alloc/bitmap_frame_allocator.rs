use alloc::vec;
use alloc::vec::Vec;
use core::mem::size_of;

use logc::trace;
use multiboot2::MemoryArea;

use super::FrameAllocator;
use crate::memory::{Address, Frame, PhysicalAddress};

pub struct BitmapFrameAllocator {
    bitmap: Vec<usize>,
    base: PhysicalAddress,
    size: usize,
}

// EVERYTHING in here is only safe if called through PhysicalMemoryManager
impl BitmapFrameAllocator {
    pub unsafe fn new(
        area: &MemoryArea,
        used_predicate: impl Fn(&usize) -> bool,
    ) -> BitmapFrameAllocator {
        let bitmap = vec![0; area.size() as usize / Frame::SIZE / size_of::<usize>() / 8];

        let mut ret = Self {
            base: PhysicalAddress::new(area.start_address() as usize),
            size: bitmap.len() * 8 * size_of::<usize>() * Frame::SIZE,
            bitmap,
        };

        // This should probably just iterate over a list of used regions and set 8
        // frames at a time...
        (PhysicalAddress::new(area.start_address() as usize)
            .next_aligned_addr(Frame::SIZE)
            .raw()
            ..=PhysicalAddress::new(area.end_address() as usize)
                .prev_aligned_addr(Frame::SIZE)
                .raw())
            .step_by(Frame::SIZE)
            .filter(used_predicate)
            .for_each(|a| ret.set_used(unsafe { PhysicalAddress::new_unchecked(a) }));

        ret
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

        if self.bitmap[field] & mask != 0 {
            panic!("Tried to set an already used address {} as used.", address);
        }

        self.bitmap[field] |= mask;

        assert!(self.bitmap[field] & mask != 0, "Failed to set.");
    }
}

impl FrameAllocator for BitmapFrameAllocator {
    fn alloc(&mut self) -> Option<Frame> {
        fn first_unset_bit(field: usize) -> usize {
            let ret: usize;
            unsafe {
                llvm_asm!("tzcnt $1, $0" : "=r"(ret) : "r"(!field));
            }
            ret
        }

        trace!("Allocating...");

        for (i, field) in self.bitmap.iter_mut().enumerate() {
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
            if self.bitmap[field] & mask == 0 {
                panic!("DOUBLE FREE");
            }

            self.bitmap[field] ^= mask;
        }
    }

    fn has(&self, frame: Frame) -> bool {
        self.base <= frame.address() && self.base + self.size > frame.address() + Frame::SIZE
    }
}
