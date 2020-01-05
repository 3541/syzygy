use core::ptr::Unique;

use super::table::{
    EntryFlags, Table, TableType, ACTIVE_TOP_LEVEL_TABLE_ADDRESS, PD, PML4, TABLE_LEVELS,
};
use super::{Page, PageSize};
use crate::memory::{Frame, FrameAllocator, FrameSize, PhysicalAddress, VirtualAddress};

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

    pub fn map_to<A: FrameAllocator>(
        &mut self,
        address: VirtualAddress,
        frame: Frame,
        flags: EntryFlags,
        allocator: &mut A,
    ) -> Page {
        assert!(address % super::FRAME_ALIGN == 0);

        trace!("Attempting to map 0x{:x} -> {:#x?}", address, frame);
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
        debug!("Going to map in kernel address space: {:#x?}", frame);
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
        fn resolve_page(tl: &Mapper, addr: VirtualAddress) -> Option<Page> {
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
