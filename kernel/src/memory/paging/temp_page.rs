use super::mapper::Mapper;
use super::table::{ActiveTopLevelTable, Table, PML4};
use super::EntryFlags;
use crate::memory::{Frame, PhysicalAddress, VirtualAddress};

pub struct TempPage(pub Frame);

impl TempPage {
    const ADDRESS: VirtualAddress = unsafe { VirtualAddress::new_const(0xe000e000) };

    pub fn map(&mut self, active: &mut ActiveTopLevelTable) {
        assert!(active.virtual_address_to_frame(Self::ADDRESS).is_none());
        active
            .map_to(Self::ADDRESS, &self.0, EntryFlags::WRITABLE)
            .flush();
    }

    pub fn unmap(self, active: &mut Mapper) -> Frame {
        active.unmap(Self::ADDRESS).flush();
        self.0
    }

    pub fn map_and_pun_frame(&mut self, active: &mut ActiveTopLevelTable) -> &mut Table<PML4> {
        self.map(active);
        unsafe { &mut *(*Self::ADDRESS as *mut Table<PML4>) }
    }

    pub fn physical_address(&self) -> PhysicalAddress {
        self.0.address()
    }

    pub fn new(frame: Frame) -> TempPage {
        TempPage(frame)
    }
}
