pub mod table;

use crate::memory::Frame;
use table::{NestedTableType, Table, TableType};

#[cfg(target_arch = "x86")]
const ENTRIES: usize = 1024;

#[cfg(target_arch = "x86_64")]
const ENTRIES: usize = 512;

#[cfg(target_arch = "x86_64")]
const PAGE_ADDR_INDEX_SHIFT: usize = 9;

#[cfg(target_arch = "x86")]
const PAGE_ADDR_INDEX_SHIFT: usize = 10;

const PAGE_ADDR_INDEX_MASK: usize = (1 << PAGE_ADDR_INDEX_SHIFT) - 1;

const PAGE_ADDR_OFFSET_SHIFT: usize = 12;
const PAGE_ADDR_OFFSET_MASK: usize = (1 << PAGE_ADDR_OFFSET_SHIFT) - 1;

pub type PhysicalAddress = usize;
pub type VirtualAddress = usize;
pub type PageSize = super::FrameSize;

pub struct Page {
    frame: Frame,
    address: VirtualAddress,
}

impl Page {
    fn containing_addr(addr: VirtualAddress) -> Self {
        #[cfg(target_arch = "x86_64")]
        assert!(
            addr < 0x0000_8000_0000_0000 || addr >= 0xFFFF_8000_0000_0000,
            "INVALID ADDRESS: 0x{:x}",
            addr
        );

        #[cfg(target_arch = "x86_64")]
        fn resolve_page(addr: VirtualAddress) -> Page {
            let tmp = Page {
                // Garbage
                frame: Frame {
                    address: 0,
                    size: super::FrameSize::Small,
                },
                address: addr,
            };
            let pdp = unsafe { &*table::PML4 }
                .next_table(tmp.pml4_index())
                .expect("Invalid PML4 entry.");

            let pd = pdp.next_table(tmp.pdp_index());
            if pd.is_none() {
                return Page {
                    frame: Frame {
                        address: pdp[addr & PAGE_ADDR_INDEX_MASK]
                            .address()
                            .expect("Unexpected non-present PDP entry."),
                        size: PageSize::Huge,
                    },
                    address: addr,
                };
            }

            let pd = pd.unwrap();
            let pt = pd.next_table(tmp.pd_index());
            if pt.is_none() {
                return Page {
                    frame: Frame {
                        address: pd[addr & PAGE_ADDR_INDEX_MASK]
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

        resolve_page(addr)
    }

    fn pml4_index(&self) -> usize {
        self.table_index(4)
    }

    fn pdp_index(&self) -> usize {
        self.table_index(3)
    }

    fn pd_index(&self) -> usize {
        self.table_index(2)
    }

    fn pt_index(&self) -> usize {
        self.table_index(1)
    }

    fn table_index(&self, n: usize) -> usize {
        (self.address >> PAGE_ADDR_OFFSET_SHIFT + PAGE_ADDR_INDEX_SHIFT * n) & PAGE_ADDR_INDEX_MASK
    }
}

pub fn translate(addr: VirtualAddress) -> PhysicalAddress {
    Page::containing_addr(addr).frame.address + addr & PAGE_ADDR_OFFSET_MASK
}
