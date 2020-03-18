use core::ptr::Unique;

use super::table::{EntryFlags, Table, ACTIVE_TOP_LEVEL_TABLE_ADDRESS, PML4, TABLE_LEVELS};
use super::Page;
use crate::constants::KERNEL_BASE;
use crate::memory::{Frame, FrameAllocator, PhysicalAddress, VirtualAddress};

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
        assert!(address % super::FRAME_SIZE == 0);

        trace!("Attempting to map 0x{:x} -> {:x?}", address, frame);
        let ret = Page { frame, address };

        let top = self.get_mut();
        //        let level = ret.size().level_index();
        let mut bottom = top
            .next_table_or_create(ret.pml4_index(), allocator)
            .next_table_or_create(ret.pdp_index(), allocator)
            .next_table_or_create(ret.pd_index(), allocator);

        /*        let indices = [ret.pdp_index(), ret.pd_index(), ret.pt_index()];

        for i in 0..(TABLE_LEVELS - /* level - */ 1) {
            assert!(!bottom[indices[i]].is_leaf());
            bottom =
                unsafe { core::mem::transmute(bottom.next_table_or_create(indices[i], allocator)) };
        }*/

        //        let index = ret.table_index(level);
        let index = ret.pt_index();
        assert!(bottom[index].is_unused());
        bottom[index].set(ret.frame.address(), flags | EntryFlags::PRESENT);

        ret
    }

    pub fn map<A: FrameAllocator>(
        &mut self,
        addr: VirtualAddress,
        flags: EntryFlags,
        allocator: &mut A,
    ) -> Page {
        self.map_to(
            addr,
            allocator.alloc().expect("Out of frames"),
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
        trace!("Going to map in kernel address space: {:x?}", frame);
        self.map_to(frame.address() + KERNEL_BASE, frame, flags, allocator)
    }

    pub fn unmap<A: FrameAllocator>(&mut self, page: Page, allocator: &mut A) {
        let mut table = self.get_mut().next_table_mut(page.pml4_index()).unwrap();
        for i in (1..(TABLE_LEVELS/*- level*/)).rev() {
            assert!(!table[page.table_index(i)].is_leaf());
            table =
                unsafe { core::mem::transmute(table.next_table_mut(page.table_index(i)).unwrap()) };
        }

        table[page.pt_index()].set_unused();
        allocator.free(page.frame);
        unsafe { asm!("invlpg $0" : : "m"(page.address())) };
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
                frame: Frame(0),
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
                    frame: Frame(address),
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
                    frame: Frame(address),
                    address: addr,
                });
            }

            let address = if let Some(a) = pt.unwrap()[tmp.pt_index()].address() {
                a
            } else {
                return None;
            };
            Some(Page {
                frame: Frame(address),
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
        self.translate_page(addr)
            .and_then(|page| Some(page.frame.address() + (addr & super::PAGE_ADDR_OFFSET_MASK)))
    }
}
