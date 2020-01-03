use core::marker::PhantomData;
use core::ops::{Index, IndexMut};
use core::ptr::Unique;

use crate::memory::paging::{Page, PageSize, PhysicalAddress, VirtualAddress};
use crate::memory::{Frame, FrameAllocator, FrameSize};

// NOTE: Magic virtual addresses
#[cfg(target_arch = "x86_64")]
pub const ACTIVE_PML4_ADDRESS: *mut Table<PML4> = 0xFFFF_FFFF_FFFF_F000 as *mut _;

#[cfg(target_arch = "x86")]
pub const ACTIVE_PD_ADDRESS: *mut Table<PD> = 0xFFFF_F000 as *mut _;

#[cfg(target_arch = "x86_64")]
pub const ACTIVE_TOP_LEVEL_TABLE_ADDRESS: *mut Table<PML4> = ACTIVE_PML4_ADDRESS;

#[cfg(target_arch = "x86")]
pub const ACTIVE_TOP_LEVEL_TABLE_ADDRESS: *mut Table<PD> = ACTIVE_PD_ADDRESS;

#[cfg(target_arch = "x86_64")]
pub const TABLE_LEVELS: usize = 3;

#[cfg(target_arch = "x86")]
pub const TABLE_LEVELS: usize = 2;

//#[cfg(target_arch = "x86_64")]
//pub const KERNEL_INDEX: usize = 511;

//#[cfg(target_arch = "x86")]
//pub const KERNEL_INDEX: usize = 768;

#[cfg(target_arch = "x86_64")]
type TopLevelTableType = PML4;

#[cfg(target_arch = "x86")]
type TopLevelTableType = PD;

pub struct ActiveTopLevelTable(Unique<Table<TopLevelTableType>>);

impl ActiveTopLevelTable {
    pub const unsafe fn new() -> ActiveTopLevelTable {
        ActiveTopLevelTable(Unique::new_unchecked(ACTIVE_TOP_LEVEL_TABLE_ADDRESS))
    }

    pub fn get(&self) -> &Table<TopLevelTableType> {
        unsafe { self.0.as_ref() }
    }

    pub fn get_mut(&mut self) -> &mut Table<TopLevelTableType> {
        unsafe { self.0.as_mut() }
    }

    pub fn map_to<A: FrameAllocator>(
        &mut self,
        address: VirtualAddress,
        frame: Frame,
        flags: EntryFlags,
        allocator: &mut A,
    ) -> Page {
        assert!(address % super::FRAME_ALIGN == 0);

        let ret = Page { frame, address };

        let top = self.get_mut();
        let level = ret.size().level_index();
        //        let t1 = top.next_table_or_create(ret.pml4_index(), allocator);
        let mut bottom = top.next_table_or_create(ret.pml4_index(), allocator);

        let indices = [ret.pdp_index(), ret.pd_index(), ret.pt_index()];

        for i in 0..(TABLE_LEVELS - level - 1) {
            assert!(!bottom[indices[i]].is_leaf());
            bottom =
                unsafe { core::mem::transmute(bottom.next_table_or_create(indices[i], allocator)) };
        }

        let index = ret.table_index(level);
        assert!(bottom[index].is_unused());
        bottom[index].set(ret.frame.address(), flags | EntryFlags::PRESENT);

        /*
                fn set_mapping(t: &mut Table<impl TableType>, p: &Page, flags: EntryFlags, level: usize) {
                    let index = p.table_index(level);
                    debug!("b2: {:x}", t as *const _ as usize);
                    assert!(t[index].is_unused());
                    t[p.table_index(level)].set(p.frame.address(), flags | EntryFlags::PRESENT);
                }

                match level {
                    2 => set_mapping(t1, &ret, flags, level),
                    1 => set_mapping(
                        t1.next_table_or_create(ret.pdp_index(), allocator),
                        &ret,
                        flags,
                        level,
                    ),
                    0 => set_mapping(
                        t1.next_table_or_create(ret.pdp_index(), allocator)
                            .next_table_or_create(ret.pd_index(), allocator),
                        &ret,
                        flags,
                        level,
                    ),
                    _ => panic!(),
                };
        */
        ret
    }

    pub fn map<A: FrameAllocator>(
        &mut self,
        addr: VirtualAddress,
        size: PageSize,
        flags: EntryFlags,
        allocator: &mut A,
    ) -> Page {
        self.map_to(
            addr,
            allocator.alloc(size).expect("Out of frames"),
            flags,
            allocator,
        )
    }

    pub fn map_kernel_space<A: FrameAllocator>(
        &mut self,
        frame: Frame,
        flags: EntryFlags,
        allocator: &mut A,
    ) -> Page {
        self.map_to(frame.address + crate::KERNEL_BASE, frame, flags, allocator)
    }

    pub fn unmap<A: FrameAllocator>(&mut self, page: Page, allocator: &mut A) {
        let level = page.size().level_index();
        fn set_unmap(table: &mut Table<impl TableType>, index: usize, address: usize) {
            table[index].set_unused();
            unsafe { asm!("invlpg $0" : : "m"(address)) };
        }

        let mut table = self.get_mut().next_table_mut(page.pml4_index()).unwrap();
        for i in (1..(TABLE_LEVELS - level)).rev() {
            assert!(!table[page.table_index(i)].is_leaf());
            table =
                unsafe { core::mem::transmute(table.next_table_mut(page.table_index(i)).unwrap()) };
        }

        table[page.table_index(level)].set_unused();
        unsafe { asm!("invlpg $0" : : "m"(page.address())) };

        /*        let res = self
            .get_mut()
            .next_table_mut(page.pml4_index())
            .and_then(|pdp| pdp.next_table_mut(page.pdp_index()))
            .and_then(|pd| pd.next_table_mut(page.pd_index()))
            .and_then(|pt| {
                set_unmap(pt, page.pt_index(), page.address());
                //                allocator.free(page.frame);
                info!("Cannot actually deallocate frame");
                Some(())
            });
        if res.is_none() {
            let pdp = self
                .get_mut()
                .next_table_mut(page.pml4_index())
                .expect("Tried to free on an unmapped PML4 entry");
            if pdp[page.pdp_index()].is_leaf() {
                set_unmap(pdp, page.pdp_index(), page.address());
            } else {
                let pd = pdp
                    .next_table_mut(page.pdp_index())
                    .expect("Tried to free on an unmapped PDP index");
                if pd[page.pd_index()].is_leaf() {
                    set_unmap(pd, page.pd_index(), page.address());
                } else {
                    panic!("Tried to unmap a nonexistent page.");
                }
            }
            info!("Cannot actually deallocate frame");
            // allocator.free(page.frame);
        }*/
    }

    pub fn translate_page(&self, addr: VirtualAddress) -> Option<Page> {
        #[cfg(target_arch = "x86_64")]
        assert!(
            addr < 0x0000_8000_0000_0000 || addr >= 0xFFFF_8000_0000_0000,
            "INVALID ADDRESS: 0x{:x}",
            addr
        );

        #[cfg(target_arch = "x86_64")]
        fn resolve_page(tl: &ActiveTopLevelTable, addr: VirtualAddress) -> Option<Page> {
            let tmp = Page {
                // Garbage
                frame: Frame {
                    address: 0,
                    size: FrameSize::Small,
                },
                address: addr,
            };
            let pdp = if let Some(t) = tl.get().next_table(tmp.pml4_index()) {
                t
            } else {
                return None;
            };

            let pd = pdp.next_table(tmp.pdp_index());
            if pd.is_none() {
                let address = if let Some(a) = pdp[tmp.pdp_index()].address() {
                    a
                } else {
                    return None;
                };
                return Some(Page {
                    frame: Frame {
                        address,
                        size: PageSize::Huge,
                    },
                    address: addr,
                });
            }

            let pd = pd.unwrap();
            let pt = pd.next_table(tmp.pd_index());
            if pt.is_none() {
                let address = if let Some(a) = pd[tmp.pd_index()].address() {
                    a
                } else {
                    return None;
                };
                return Some(Page {
                    frame: Frame {
                        address,
                        size: PageSize::Large,
                    },
                    address: addr,
                });
            }

            let address = if let Some(a) = pt.unwrap()[tmp.pt_index()].address() {
                a
            } else {
                return None;
            };
            Some(Page {
                frame: Frame {
                    address,
                    size: PageSize::Small,
                },
                address: addr,
            })
        }

        #[cfg(target_arch = "x86")]
        fn resolve_page(tl: &ActiveTopLevelTable, addr: VirtualAddress) -> Page {
            let tmp = Page {
                // Garbage
                frame: Frame {
                    address: 0,
                    size: super::FrameSize::Small,
                },
                address: addr,
            };

            let pd = unsafe { tl.get() };
            let pt = pd.next_table(tmp.pd_index());
            if pt.is_none() {
                return Page {
                    frame: Frame {
                        address: pd[tmp.pd_index()]
                            .address()
                            .expect("Unexpected non-present PD entry."),
                        size: PageSize::Large,
                    },
                    address: addr,
                };
            }

            Page {
                frame: Frame {
                    address: pt.unwrap()[addr & PAGE_ADDR_INDEX_MASK]
                        .address()
                        .expect("Unexpected non-present PT entry."),
                    size: PageSize::Small,
                },
                address: addr,
            }
        }

        resolve_page(&self, addr)
    }

    pub fn translate(&self, addr: VirtualAddress) -> Option<PhysicalAddress> {
        self.translate_page(addr).and_then(|page| {
            Some(page.frame.address + (addr & super::page_addr_offset_mask(page.size())))
        })
    }
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
            Some(self.0 & Self::ADDRESS_MASK)
        } else {
            None
        }
    }

    pub fn set(&mut self, address: usize, flags: EntryFlags) {
        trace!("ENTERED set");
        debug!(
            "Address: {:#x}, address & !ADDRESS_MASK: {:#x}",
            address,
            address & !Self::ADDRESS_MASK
        );
        assert_eq!(address & !Self::ADDRESS_MASK, 0);
        self.0 = address | flags.bits();
    }

    fn set_unused(&mut self) {
        self.0 = 0;
    }

    pub fn is_unused(&self) -> bool {
        !self.flags().contains(EntryFlags::PRESENT)
    }

    pub fn is_leaf(&self) -> bool {
        self.flags().contains(EntryFlags::LARGE)
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

    pub fn next_table_or_create<A: FrameAllocator>(
        &mut self,
        index: usize,
        allocator: &mut A,
    ) -> &mut Table<T::EntryType> {
        match self.next_table(index) {
            Some(_) => self.next_table_mut(index).unwrap(),
            None => {
                if self.entries[index].flags().contains(EntryFlags::LARGE) {
                    panic!("Tried to take next table on an entry pointing to a huge page")
                } else {
                    debug!(
                        "Creating new table at {} in {:#x}",
                        index,
                        (self as *mut _) as usize
                    );
                    let f = allocator
                        .alloc(FrameSize::Small)
                        .expect("No frames available?");
                    self.entries[index].set(f.address, EntryFlags::PRESENT | EntryFlags::WRITABLE);
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
