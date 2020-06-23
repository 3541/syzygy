use core::mem::forget;
use core::ptr::Unique;

use logc::trace;

use super::table::{EntryFlags, Table, ACTIVE_TOP_LEVEL_TABLE_ADDRESS, PML4, PT};
use crate::constants::KERNEL_BASE;
use crate::memory::{Address, Frame, PhysicalAddress, VirtualAddress, PHYSICAL_ALLOCATOR};

#[cfg(target_arch = "x86_64")]
type TopLevelTableType = PML4;

#[cfg(target_arch = "x86")]
type TopLevelTableType = PD;

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

    pub fn map_to(&mut self, address: VirtualAddress, frame: Frame, flags: EntryFlags) -> Page {
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

        if flags.contains(EntryFlags::WRITABLE) {
            let new_mem = unsafe { &mut *(*address as *mut u8) };
            let mut old = *new_mem;
            core::mem::swap(new_mem, &mut 8);
            core::mem::swap(new_mem, &mut old);
            assert_eq!(
                old, 8,
                "Can't write to just-mapped writable page. Something is up.\ntable: {:x?}, index: {}", table, index
            );
        }

        MapperResult(address)
    }

    pub fn map(&mut self, address: VirtualAddress, flags: EntryFlags) -> (MapperResult, Frame) {
        let frame = PHYSICAL_ALLOCATOR.alloc_frame().expect("Out of frames");
        (self.map_to(address, &frame, flags), frame)
    }

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

    pub fn virtual_address_to_frame(&self, addr: VirtualAddress) -> Option<Frame> {
        #[cfg(target_arch = "x86_64")]
        fn resolve_page(tl: &Mapper, address: VirtualAddress) -> Option<Frame> {
            Some(Frame(
                tl.get_bottom_table(address)?[address.pt_index()].address()?,
            ))
        }

        #[cfg(target_arch = "x86")]
        fn resolve_page(tl: &ActiveTopLevelTable, addr: VirtualAddress) -> Page {
            unimplemented!()
        }

        resolve_page(&self, addr)
    }

    pub fn translate(&self, addr: VirtualAddress) -> Option<PhysicalAddress> {
        self.virtual_address_to_frame(addr)
            .map(|frame| frame.address() + addr.offset_into_frame())
    }
}
