use core::fmt;
use core::marker::PhantomData;
use core::mem::forget;
use core::ops::{Deref, DerefMut, Index, IndexMut};

use bitflags::bitflags;
use logc::trace;
use multiboot2::{ElfSection, ElfSectionFlags};

use super::mapper::Mapper;
use super::temp_page::TempPage;
use crate::memory::{
    Address, Frame, PhysicalAddress, RawVirtualAddress, VirtualAddress, PHYSICAL_ALLOCATOR,
};

// NOTE: Magic virtual addresses
#[cfg(target_arch = "x86_64")]
pub const ACTIVE_PML4_ADDRESS: *mut Table<PML4> = 0xFFFF_FFFF_FFFF_F000 as *mut _;

pub const RECURSIVE_MAPPING_BASE: VirtualAddress =
    unsafe { VirtualAddress::new_const(0xFFFF_FF80_0000_0000) };

#[cfg(target_arch = "x86")]
pub const ACTIVE_PD_ADDRESS: *mut Table<PD> = 0xFFFF_F000 as *mut _;

#[cfg(target_arch = "x86_64")]
pub const ACTIVE_TOP_LEVEL_TABLE_ADDRESS: *mut Table<PML4> = ACTIVE_PML4_ADDRESS;

#[cfg(target_arch = "x86")]
pub const ACTIVE_TOP_LEVEL_TABLE_ADDRESS: *mut Table<PD> = ACTIVE_PD_ADDRESS;

//#[cfg(target_arch = "x86_64")]
//pub const KERNEL_INDEX: usize = 511;

//#[cfg(target_arch = "x86")]
//pub const KERNEL_INDEX: usize = 768;
//

pub struct ActiveTopLevelTable(Mapper);

impl Deref for ActiveTopLevelTable {
    type Target = Mapper;

    fn deref(&self) -> &Mapper {
        &self.0
    }
}

impl DerefMut for ActiveTopLevelTable {
    fn deref_mut(&mut self) -> &mut Mapper {
        &mut self.0
    }
}

impl ActiveTopLevelTable {
    pub unsafe fn new() -> ActiveTopLevelTable {
        ActiveTopLevelTable(Mapper::new())
    }

    pub fn with(&mut self, table: &mut InactiveTopLevelTable, then: impl FnOnce(&mut Mapper)) {
        let prev_pml4_address: usize;
        unsafe { llvm_asm!("mov %cr3, %rax" : "={rax}"(prev_pml4_address) ::: "volatile") };
        let prev_pml4_address = PhysicalAddress::new(prev_pml4_address);

        let mut temp = TempPage::new(Frame(prev_pml4_address));
        let prev_pml4 = temp.map_and_pun_frame(self);

        trace!("NEW TABLE IS AT 0x{:x?}", table.address());

        self.get_mut()[511].set(table.address(), EntryFlags::PRESENT | EntryFlags::WRITABLE);

        super::flush_tlb();

        then(self);

        prev_pml4[511].set(
            prev_pml4_address,
            EntryFlags::PRESENT | EntryFlags::WRITABLE,
        );

        super::flush_tlb();
        forget(temp.0);
    }

    pub fn switch(&mut self, new: InactiveTopLevelTable) -> InactiveTopLevelTable {
        let mut cr3: usize;
        unsafe { llvm_asm!("mov %cr3, %rax" : "={rax}"(cr3) ::: "volatile") };
        let old = InactiveTopLevelTable(Frame(PhysicalAddress::new(cr3)));
        trace!("Read {} from cr3", old.address());

        trace!("Writing 0x{:x} to cr3", *new.address());
        unsafe { llvm_asm!("mov %rax, %cr3" :: "{rax}"(*new.address()) :: "volatile") };
        forget(new.0);

        old
    }
}

pub struct InactiveTopLevelTable(Frame);

impl InactiveTopLevelTable {
    pub fn new(active: &mut ActiveTopLevelTable, mut temp: TempPage) -> InactiveTopLevelTable {
        let phys = temp.physical_address();
        let table = temp.map_and_pun_frame(active);
        table.zero();
        table[511].set(phys, EntryFlags::PRESENT | EntryFlags::WRITABLE);
        InactiveTopLevelTable(temp.unmap(active))
    }

    pub fn into_frame(self) -> Frame {
        self.0
    }

    pub fn address(&self) -> PhysicalAddress {
        self.0.address()
    }
}

pub enum TopLevelTable {
    Active(ActiveTopLevelTable),
    Inactive(InactiveTopLevelTable),
}

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

impl EntryFlags {
    pub fn from_elf(section: &ElfSection) -> EntryFlags {
        let mut ret = EntryFlags::empty();

        let flags = section.flags();

        if flags.contains(ElfSectionFlags::ALLOCATED) {
            ret |= EntryFlags::PRESENT;
        }
        if flags.contains(ElfSectionFlags::WRITABLE) {
            ret |= EntryFlags::WRITABLE;
        }
        if !flags.contains(ElfSectionFlags::EXECUTABLE) {
            ret |= EntryFlags::NO_EXECUTE;
        }

        ret
    }
}

#[derive(Debug, Clone)]
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
            Some(PhysicalAddress::new(self.0 & Self::ADDRESS_MASK))
        } else {
            None
        }
    }

    pub fn set(&mut self, address: PhysicalAddress, flags: EntryFlags) {
        trace!("ENTERED set");
        trace!("Address: {}, Entry: {:x?}", address, self);
        assert_eq!(*address & !Self::ADDRESS_MASK, 0);
        self.0 = *address | flags.bits();
        trace!("Self is now {:x?}", self);
    }

    pub fn set_unused(&mut self) {
        self.0 = 0;
    }

    pub fn is_unused(&self) -> bool {
        !self.flags().contains(EntryFlags::PRESENT)
    }

    /*    pub fn is_leaf(&self) -> bool {
        self.flags().contains(EntryFlags::LARGE)
    }*/
}

pub struct Table<T: TableType + ?Sized> {
    entries: [Entry; Table::<PML4>::ENTRIES],
    t: PhantomData<T>,
}

impl<T: TableType> fmt::Debug for Table<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Table @ {:?}", self as *const _)
    }
}

impl<T: TableType> Table<T> {
    #[cfg(target_arch = "x86")]
    const ENTRIES: usize = 1024;

    #[cfg(target_arch = "x86_64")]
    const ENTRIES: usize = 512;

    pub fn zero(&mut self) {
        for e in self.entries.iter_mut() {
            e.set_unused()
        }
    }
}

impl<T: TableType + NestedTableType> Table<T> {
    pub fn next_table_addr(&self, index: usize) -> Option<VirtualAddress> {
        let flags = self[index].flags();
        if flags.contains(EntryFlags::PRESENT) && !flags.contains(EntryFlags::LARGE) {
            Some(VirtualAddress::new(
                ((self as *const _ as RawVirtualAddress) << VirtualAddress::PAGE_ADDR_INDEX_SHIFT)
                    | (index << 12),
            ))
        } else {
            None
        }
    }

    pub fn next_table(&self, index: usize) -> Option<&Table<T::EntryType>> {
        self.next_table_addr(index)
            .map(|a| unsafe { &*(*a as *const _) })
    }

    pub fn next_table_mut(&mut self, index: usize) -> Option<&mut Table<T::EntryType>> {
        self.next_table_addr(index)
            .map(|a| unsafe { &mut *(*a as *mut _) })
    }

    pub fn next_table_or_create(&mut self, index: usize) -> &mut Table<T::EntryType> {
        match self.next_table(index) {
            Some(_) => self.next_table_mut(index).unwrap(),
            None => {
                if self.entries[index].flags().contains(EntryFlags::LARGE) {
                    panic!("Tried to take next table on an entry pointing to a huge page")
                } else {
                    trace!(
                        "Creating new table at {} in {:#x}",
                        index,
                        (self as *mut _) as usize
                    );
                    let f = PHYSICAL_ALLOCATOR
                        .alloc_frame()
                        .expect("No frames available?");
                    trace!("Got frame");
                    self.entries[index]
                        .set(f.address(), EntryFlags::PRESENT | EntryFlags::WRITABLE);
                    // FIXME: Deal with Table ownership intelligently
                    forget(f);
                    let t = self.next_table_mut(index).unwrap();
                    t.zero();
                    t
                }
            }
        }
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
#[derive(Debug)]
pub enum PML4 {}
#[derive(Debug)]
pub enum PDP {}
#[derive(Debug)]
pub enum PD {}
#[derive(Debug)]
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
