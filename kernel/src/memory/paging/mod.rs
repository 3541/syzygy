pub mod mapper;
pub mod table;
mod temp_page;

use core::cmp::{max, min};

use logc::{debug, trace};
use multiboot2::ElfSectionIter;

pub use table::{ActiveTopLevelTable, EntryFlags};

use crate::constants::KERNEL_BASE;
use crate::memory::{
    Address, Frame, FrameAllocator, PhysicalAddress, VirtualAddress, FRAME_ALLOCATOR, FRAME_SIZE,
};
use table::InactiveTopLevelTable;
use temp_page::TempPage;

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

#[derive(Debug, Clone, Copy)]
pub struct Page(VirtualAddress);

impl Page {
    #[cfg(target_arch = "x86_64")]
    pub fn pml4_index(&self) -> usize {
        self.table_index(3)
    }

    pub fn pdp_index(&self) -> usize {
        self.table_index(2)
    }

    pub fn pd_index(&self) -> usize {
        self.table_index(1)
    }

    pub fn pt_index(&self) -> usize {
        self.table_index(0)
    }

    // NOTE: 0-indexed (PT is 0, PML4 is 3)
    fn table_index(&self, n: usize) -> usize {
        // NOTE: it should be okay to use OFFSET_SHIFT like this, even though it's
        // sort of broken for larger pages, because the total offset is still the same
        // if we want some specific table. e.g., PML4 index is always at the same place.
        (*self.0 >> PAGE_ADDR_OFFSET_SHIFT + PAGE_ADDR_INDEX_SHIFT * n) & PAGE_ADDR_INDEX_MASK
    }

    pub fn address(&self) -> VirtualAddress {
        self.0
    }

    #[inline]
    unsafe fn flush(&self) {
        llvm_asm!("invlpg ($0)" :: "r"(*self.address()) : "memory");
    }
}

fn flush_tlb() {
    unsafe {
        let mut cr3: usize;
        llvm_asm!("mov %cr3, %rax" : "={rax}"(cr3) ::: "volatile");
        llvm_asm!("mov %rax, %cr3" :: "{rax}"(cr3) :: "volatile");
    }
}

// make new PDP, PT tables, map to kernel
pub fn remap_kernel(
    top: &mut ActiveTopLevelTable,
    elf_sections: ElfSectionIter,
    multiboot_info: &multiboot2::BootInformation,
    initramfs_start: PhysicalAddress,
    initramfs_end: PhysicalAddress,
) {
    let frame = FRAME_ALLOCATOR
        .lock()
        .alloc()
        .expect("Need free frame to remap kernel.");
    let temp = TempPage::new(Page(VirtualAddress::new(0xe000e000)), frame);
    let mut new_table = InactiveTopLevelTable::new(top, temp);

    let mut temp = TempPage::new(
        Page(VirtualAddress::new(0xe000e000)),
        FRAME_ALLOCATOR.lock().alloc().unwrap(),
    );

    top.with(&mut new_table, &mut temp, |m| {
        debug!("Mapping kernel sections");
        for section in elf_sections {
            if !section.is_allocated()
                || VirtualAddress::new(section.start_address() as usize) < KERNEL_BASE
            {
                continue;
            }
            assert!(
                section.start_address() as usize % FRAME_SIZE == 0,
                "Kernel sections must be 4K aligned."
            );

            trace!(
                "Mapping section 0x{:x}-0x{:x} (0x{:x})",
                section.start_address(),
                section.end_address(),
                section.size()
            );

            let from = Frame(PhysicalAddress::new(
                (section.start_address() as usize) - *KERNEL_BASE,
            ));
            let to = Frame(PhysicalAddress::new(
                (section.end_address() as usize - 1)
                    - (section.end_address() as usize - 1) % FRAME_SIZE
                    - *KERNEL_BASE,
            ));

            let flags = EntryFlags::from_elf(&section);

            for frame in Frame::range_inclusive(from, to) {
                m.map_kernel_space(frame, flags);
            }
        }

        debug!("Mapping VGA buffer");
        m.map_kernel_space(
            Frame(crate::vga_text::VGA_BUFFER_ADDRESS),
            EntryFlags::WRITABLE,
        );

        debug!("Mapping Multiboot info structure and initramfs");
        let from = Frame::containing_address(min(
            PhysicalAddress::new(multiboot_info.start_address() - *KERNEL_BASE),
            initramfs_start,
        ));
        let to = Frame::containing_address(max(
            PhysicalAddress::new(multiboot_info.end_address() - *KERNEL_BASE - 1),
            initramfs_end,
        ));

        for frame in Frame::range_inclusive(from, to) {
            m.map_kernel_space(frame, EntryFlags::PRESENT);
        }
    });

    debug!("Finished remapping.");
    let old = top.switch(new_table);
    debug!("Switched to new table.");

    FRAME_ALLOCATOR.lock().free(old.frame());
    let guard = Page((KERNEL_BASE + *old.address()).into());
    top.unmap(guard);
    debug!("Guard page at {}", KERNEL_BASE + *old.address());
}
