use core::mem::forget;
use core::ptr::Unique;

use logc::{trace, warn};

use super::table::{EntryFlags, Table, ACTIVE_TOP_LEVEL_TABLE_ADDRESS, PML4, PT};
use crate::constants::KERNEL_BASE;
use crate::memory::{Address, Frame, VirtualAddress};

#[cfg(target_arch = "x86_64")]
type TopLevelTableType = PML4;

#[cfg(target_arch = "x86")]
type TopLevelTableType = PD;

#[must_use = "A flush is necessary."]
pub struct MapperResult(VirtualAddress);

impl MapperResult {
    pub fn flush(self) {
        unsafe { asm!("invlpg [{0}]", in(reg) *self.0) };
        forget(self);
    }
}

impl Drop for MapperResult {
    fn drop(&mut self) {
        panic!("Unflushed mapping.");
    }
}

#[must_use = "A flush must be invoked with .flush()"]
pub struct TLBFlush(bool);

impl TLBFlush {
    pub fn new() -> Self {
        TLBFlush(false)
    }

    pub fn consume(&mut self, result: MapperResult) {
        self.0 = true;
        forget(result);
    }

    pub fn consume_other(&mut self, other: TLBFlush) {
        if other.0 {
            self.0 = true;
        }
        forget(other);
    }

    pub fn flush(self) {
        // Reloading CR3 flushes the TLB completely.
        if self.0 {
            unsafe {
                asm!("mov rax, cr3");
                asm!("mov cr3, rax");
            };
        } else {
            warn!("Unnecessary TLBFlush -- never consumed a MapperResult.");
        }

        forget(self);
    }
}

impl Drop for TLBFlush {
    fn drop(&mut self) {
        if self.0 {
            panic!("Unused TLBFlush.");
        } else {
            warn!("Dropping unnecessary TLBFlush.");
        }
    }
}

pub struct Mapper(Unique<Table<TopLevelTableType>>);

impl Mapper {
    pub unsafe fn new() -> Mapper {
        Mapper(Unique::new_unchecked(ACTIVE_TOP_LEVEL_TABLE_ADDRESS))
    }

    pub fn get(&self) -> &Table<TopLevelTableType> {
        unsafe { self.0.as_ref() }
    }

    pub fn get_mut(&mut self) -> &mut Table<TopLevelTableType> {
        unsafe { self.0.as_mut() }
    }

    fn get_bottom_table_or_create(&mut self, address: VirtualAddress) -> &mut Table<PT> {
        self.get_mut()
            .next_table_or_create(address.pml4_index())
            .next_table_or_create(address.pdp_index())
            .next_table_or_create(address.pd_index())
    }

    fn get_bottom_table_mut(&mut self, address: VirtualAddress) -> Option<&mut Table<PT>> {
        self.get_mut()
            .next_table_mut(address.pml4_index())
            .and_then(|t| t.next_table_mut(address.pdp_index()))
            .and_then(|t| t.next_table_mut(address.pd_index()))
    }

    fn get_bottom_table(&self, address: VirtualAddress) -> Option<&Table<PT>> {
        self.get()
            .next_table(address.pml4_index())
            .and_then(|t| t.next_table(address.pdp_index()))
            .and_then(|t| t.next_table(address.pd_index()))
    }

    pub fn map_to(
        &mut self,
        address: VirtualAddress,
        frame: &Frame,
        flags: EntryFlags,
    ) -> MapperResult {
        assert!(address.is_aligned(Frame::SIZE));

        trace!("Attempting to map {} -> {}", address, frame);

        let table = self.get_bottom_table_or_create(address);
        let index = address.pt_index();
        assert!(
            table[index].is_unused(),
            "Bottom wasn't unused when trying to map {:x?} to {}",
            frame,
            address
        );
        table[index].set(frame.address(), flags | EntryFlags::PRESENT);

        /*        if flags.contains(EntryFlags::WRITABLE) {
            let new_mem = unsafe { &mut *(*address as *mut u8) };
            let mut old = *new_mem;
            core::mem::swap(new_mem, &mut 8);
            core::mem::swap(new_mem, &mut old);
            assert_eq!(
                old, 8,
                "Can't write to just-mapped writable page. Something is up.\ntable: {:x?}, index: {}", table, index
            );
        }*/

        MapperResult(address)
    }

    /*  pub fn map(&mut self, address: VirtualAddress, flags: EntryFlags) -> (MapperResult, Frame) {
        let frame = PHYSICAL_ALLOCATOR.alloc_frame().expect("Out of frames");
        (self.map_to(address, &frame, flags), frame)
    }*/

    // This is an evil function. It's not just unsafe, it's actively malicious.
    // It should only /ever/ be used when first remapping the kernel. It will,
    // thankfully, cause a panic in the case of an attempted double-mapping, but
    // that is obviously not ideal.
    pub unsafe fn map_kernel_space(&mut self, frame: &Frame, flags: EntryFlags) -> MapperResult {
        trace!("Going to map in kernel address space: {}", frame);
        let address = VirtualAddress::new(*(frame.address() + *KERNEL_BASE));
        self.map_to(address, frame, flags)
    }

    pub fn unmap(&mut self, address: VirtualAddress) -> MapperResult {
        let table = self
            .get_bottom_table_mut(address)
            .expect("Tried to unmap a page for which there is no table.");
        table[address.pt_index()].set_unused();
        MapperResult(address)
    }

    pub fn virtual_address_to_frame(&self, address: VirtualAddress) -> Option<Frame> {
        #[cfg(target_arch = "x86_64")]
        fn resolve_page(tl: &Mapper, address: VirtualAddress) -> Option<Frame> {
            Some(Frame(
                tl.get_bottom_table(address)?[address.pt_index()].address()?,
            ))
        }

        #[cfg(target_arch = "x86")]
        fn resolve_page(tl: &ActiveTopLevelTable, address: VirtualAddress) -> Page {
            unimplemented!()
        }

        resolve_page(&self, address)
    }

    /*
    pub fn translate(&self, address: VirtualAddress) -> Option<PhysicalAddress> {
        self.virtual_address_to_frame(address)
            .map(|frame| frame.address() + address.offset_into_frame())
    }*/
}
