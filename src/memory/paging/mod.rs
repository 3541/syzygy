pub mod table;

use multiboot2::ElfSectionIter;

pub use table::{ActiveTopLevelTable, EntryFlags};

use crate::memory::{Frame, FrameAllocator, FRAME_ALIGN};

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
//const PAGE_ADDR_OFFSET_MASK: usize = (1 << PAGE_ADDR_OFFSET_SHIFT) - 1;
#[inline]
fn page_addr_offset_mask(size: PageSize) -> usize {
    (1 << PAGE_ADDR_OFFSET_SHIFT
        * match size {
            PageSize::Small => 1,
            PageSize::Large => 2,
            #[cfg(target_arch = "x86_64")]
            PageSize::Huge => 3,
        })
        - 1
}

pub type PhysicalAddress = usize;
pub type VirtualAddress = usize;
pub type PageSize = super::FrameSize;

#[derive(Debug, Clone)]
pub struct Page {
    frame: Frame,
    address: VirtualAddress,
}

impl Page {
    #[cfg(target_arch = "x86_64")]
    fn pml4_index(&self) -> usize {
        self.table_index(3)
    }

    fn pdp_index(&self) -> usize {
        self.table_index(2)
    }

    fn pd_index(&self) -> usize {
        self.table_index(1)
    }

    fn pt_index(&self) -> usize {
        self.table_index(0)
    }

    // NOTE: 0-indexed (PT is 0, PML4 is 3)
    fn table_index(&self, n: usize) -> usize {
        // NOTE: it should be okay to use OFFSET_SHIFT like this, even though it's
        // sort of broken for larger pages, because the total offset is still the same
        // if we want some specific table. e.g., PML4 index is always at the same place.
        (self.address >> PAGE_ADDR_OFFSET_SHIFT + PAGE_ADDR_INDEX_SHIFT * n) & PAGE_ADDR_INDEX_MASK
    }

    pub fn size(&self) -> PageSize {
        self.frame.size
    }

    pub fn address(&self) -> VirtualAddress {
        self.address
    }
}

pub fn remap_kernel<A: FrameAllocator>(allocator: &mut A, elf_tag: ElfSectionIter) {
    // make new PDP, PT tables, map to kernel

    unimplemented!()
}
