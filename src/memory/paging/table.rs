use core::marker::PhantomData;
use core::ops::{Index, IndexMut};

use crate::memory::paging::PhysicalAddress;

#[cfg(target_arch = "x86_64")]
pub const ACTIVE_PML4: *mut Table<PML4> = 0xFFFF_FFFF_FFFF_F000 as *mut _;

#[cfg(target_arch = "x86")]
pub const ACTIVE_PD: *mut Table<PD> = 0xFFFF_F000 as *mut _;

#[cfg(target_arch = "x86_64")]
pub const ACTIVE_TOP_LEVEL_TABLE: *mut Table<PML4> = ACTIVE_PML4;

#[cfg(target_arch = "x86")]
pub const ACTIVE_TOP_LEVEL_TABLE: *mut Table<PD> = ACTIVE_PD;

//#[cfg(target_arch = "x86_64")]
//pub const KERNEL_INDEX: usize = 511;

//#[cfg(target_arch = "x86")]
//pub const KERNEL_INDEX: usize = 768;

bitflags! {
    pub struct EntryFlags: usize {
        const PRESENT = 1;
        const WRITABLE = 1 << 1;
        const USER_ACCESSIBLE = 1 << 2;
        const WRITE_THROUGH = 1 << 3;
        const CACHE_DISABLED = 1 << 4;
        const ACCESSED = 1 << 5;
        const DIRTY = 1 << 6;
        const LARGE = 1 << 7;
        const GLOBAL = 1 << 8;
        #[cfg(target_arch = "x86_64")]
        const NO_EXECUTE = 1 << 63;
    }
}

#[derive(Debug)]
pub struct Entry(usize);

impl Entry {
    #[cfg(target_arch = "x86")]
    const ADDRESS_MASK: usize = 0xFFF_F000;

    #[cfg(target_arch = "x86_64")]
    const ADDRESS_MASK: usize = 0x000F_FFFF_FFFF_F000;

    pub fn flags(&self) -> EntryFlags {
        EntryFlags::from_bits_truncate(self.0)
    }

    pub fn address(&self) -> Option<PhysicalAddress> {
        if self.flags().contains(EntryFlags::PRESENT) {
            Some(self.0 & Self::ADDRESS_MASK)
        } else {
            None
        }
    }

    pub fn set(&mut self, address: usize, flags: EntryFlags) {
        assert_eq!(address ^ Self::ADDRESS_MASK, 0);
        self.0 = address | flags.bits();
    }

    fn set_unused(&mut self) {
        self.0 = 0;
    }
}

pub struct Table<T: TableType> {
    entries: [Entry; super::ENTRIES],
    t: PhantomData<T>,
}

impl<T: TableType> Table<T> {
    pub fn zero(&mut self) {
        for e in self.entries.iter_mut() {
            e.set_unused()
        }
    }
}

impl<T: TableType + NestedTableType> Table<T> {
    pub fn next_table_addr(&self, index: usize) -> Option<PhysicalAddress> {
        let flags = self[index].flags();
        if flags.contains(EntryFlags::PRESENT) && !flags.contains(EntryFlags::LARGE) {
            Some(
                ((self as *const _ as PhysicalAddress) << super::PAGE_ADDR_INDEX_SHIFT)
                    | (index << 12),
            )
        } else {
            None
        }
    }

    pub fn next_table(&self, index: usize) -> Option<&Table<T::EntryType>> {
        self.next_table_addr(index)
            .map(|a| unsafe { &*(a as *const _) })
    }

    pub fn next_table_mut(&mut self, index: usize) -> Option<&mut Table<T::EntryType>> {
        self.next_table_addr(index)
            .map(|a| unsafe { &mut *(a as *mut _) })
    }
}

impl<T: TableType> Index<usize> for Table<T> {
    type Output = Entry;

    fn index(&self, index: usize) -> &Entry {
        &self.entries[index]
    }
}

impl<T: TableType> IndexMut<usize> for Table<T> {
    fn index_mut(&mut self, index: usize) -> &mut Entry {
        &mut self.entries[index]
    }
}

pub trait TableType {}
pub trait NestedTableType {
    type EntryType: TableType;
}

#[cfg(target_arch = "x86_64")]
pub enum PML4 {}
pub enum PDP {}
pub enum PD {}
pub enum PT {}

#[cfg(target_arch = "x86_64")]
impl TableType for PML4 {}
impl TableType for PDP {}
impl TableType for PD {}
impl TableType for PT {}

#[cfg(target_arch = "x86_64")]
impl NestedTableType for PML4 {
    type EntryType = PDP;
}
impl NestedTableType for PDP {
    type EntryType = PD;
}
impl NestedTableType for PD {
    type EntryType = PT;
}
