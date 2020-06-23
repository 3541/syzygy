pub mod mapper;
pub mod table;
mod temp_page;

use core::cmp::{max, min};
use core::mem::forget;

use logc::{debug, trace};
use multiboot2::ElfSectionIter;

pub use table::{ActiveTopLevelTable, EntryFlags, TopLevelTable};

use super::region::{VirtualRegion, VirtualRegionAllocator};
use super::{Address, Frame, PhysicalAddress, PhysicalMemory, VirtualAddress, PHYSICAL_ALLOCATOR};
use crate::constants::KERNEL_BASE;
use mapper::Mapper;
use table::InactiveTopLevelTable;
use temp_page::TempPage;

pub struct Pager {
    table: TopLevelTable,
    allocator: VirtualRegionAllocator<{ Frame::SIZE }>,
}

impl Pager {
    pub fn new(table: TopLevelTable, region: VirtualRegion) -> Self {
        Pager {
            table,
            allocator: VirtualRegionAllocator::new(region),
        }
    }

    pub fn allocator(&mut self) -> &mut VirtualRegionAllocator<{ Frame::SIZE }> {
        &mut self.allocator
    }

    pub fn mapper(&mut self) -> &mut Mapper {
        match &mut self.table {
            TopLevelTable::Active(t) => &mut *t,
            _ => panic!("Tried to get mapper on an inactive table, somehow."),
        }
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
pub unsafe fn remap_kernel(
    top: &mut ActiveTopLevelTable,
    elf_sections: ElfSectionIter,
    multiboot_info: &multiboot2::BootInformation,
    initramfs_start: PhysicalAddress,
    initramfs_end: PhysicalAddress,
) {
    let frame = PHYSICAL_ALLOCATOR
        .alloc_frame()
        .expect("Need free frame to remap kernel.");
    let temp = TempPage::new(frame);
    let mut new_table = InactiveTopLevelTable::new(top, temp);

    top.with(&mut new_table, |m| {
        debug!("Mapping kernel sections");
        for section in elf_sections {
            if !section.is_allocated()
                || VirtualAddress::new(section.start_address() as usize) < KERNEL_BASE
            {
                continue;
            }
            assert!(
                section.start_address() as usize % Frame::SIZE == 0,
                "Kernel sections must be 4K aligned."
            );

            trace!(
                "Mapping section 0x{:x}-0x{:x} (0x{:x})",
                section.start_address(),
                section.end_address(),
                section.size()
            );

            let from = PhysicalAddress::new((section.start_address() as usize) - *KERNEL_BASE);
            let to = PhysicalAddress::new(
                (section.end_address() as usize - 1)
                    - (section.end_address() as usize - 1) % Frame::SIZE
                    - *KERNEL_BASE,
            );

            let flags = EntryFlags::from_elf(&section);

            for frame in PhysicalMemory::region(from, to).into_frames() {
                m.map_kernel_space(&frame, flags);
                forget(frame);
            }
        }

        debug!("Mapping VGA buffer");
        let vga_buffer = Frame(crate::vga_text::VGA_BUFFER_ADDRESS);
        m.map_kernel_space(&vga_buffer, EntryFlags::WRITABLE);
        forget(vga_buffer);

        debug!("Mapping Multiboot info structure and initramfs");
        let from = min(
            PhysicalAddress::new(multiboot_info.start_address() - *KERNEL_BASE),
            initramfs_start,
        )
        .previous_aligned(Frame::SIZE);
        let to = max(
            PhysicalAddress::new(multiboot_info.end_address() - *KERNEL_BASE - 1),
            initramfs_end,
        )
        .next_aligned(Frame::SIZE);

        for frame in PhysicalMemory::region(from, to).into_frames() {
            m.map_kernel_space(&frame, EntryFlags::PRESENT);
            forget(frame);
        }
    });

    debug!("Finished remapping.");
    let old = top.switch(new_table);
    debug!("Switched to new table.");

    //    PHYSICAL_ALLOCATOR.free(old.frame());
    let guard = KERNEL_BASE + *old.address();
    top.unmap(guard);
    debug!("Guard page at {}", KERNEL_BASE + *old.address());

    forget(old.into_frame());
}
