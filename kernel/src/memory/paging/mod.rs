pub mod mapper;
pub mod table;
mod temp_page;

use core::mem::forget;

use logc::{debug, trace};
use multiboot2::ElfSectionIter;

pub use table::{ActiveTopLevelTable, EntryFlags, TopLevelTable};

use super::region::{VirtualRegion, VirtualRegionAllocator};
use super::{Address, Frame, PhysicalAddress, PhysicalMemory, VirtualAddress, PHYSICAL_ALLOCATOR};
use crate::constants::KERNEL_BASE;
use mapper::{Mapper, TLBFlush};
use table::InactiveTopLevelTable;
use temp_page::TempPage;

pub struct Pager {
    table: TopLevelTable,
    kernel_allocator: VirtualRegionAllocator<{ Frame::SIZE }>,
}

impl Pager {
    pub fn new(table: TopLevelTable, region: VirtualRegion) -> Self {
        Pager {
            table,
            kernel_allocator: VirtualRegionAllocator::new(region),
        }
    }

    pub fn kernel_allocator(&mut self) -> &mut VirtualRegionAllocator<{ Frame::SIZE }> {
        &mut self.kernel_allocator
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
) {
    let frame = PHYSICAL_ALLOCATOR
        .alloc_frame()
        .expect("Need free frame to remap kernel.");
    let temp = TempPage::new(frame);
    let mut new_table = InactiveTopLevelTable::new(top, temp);
    let mut tlb_flush = TLBFlush::new();

    top.with(&mut new_table, |m| {
        let mut closure_tlb_flush = TLBFlush::new();
        let mut map_region_in_kernel =
            |m: &mut Mapper, address: PhysicalAddress, size: usize, flags: EntryFlags| {
                for frame in PhysicalMemory::region(address, size).into_frames() {
                    closure_tlb_flush.consume(m.map_kernel_space(&frame, flags));
                    forget(frame);
                }
            };

        debug!("Mapping kernel sections.");
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
                "Mapping section 0x{:x}-0x{:x} (0x{:x}).",
                section.start_address(),
                section.end_address(),
                section.size()
            );

            map_region_in_kernel(
                m,
                PhysicalAddress::new((section.start_address() as usize) - *KERNEL_BASE),
                (section.end_address() - section.start_address()) as usize,
                EntryFlags::from_elf(&section),
            );
        }

        debug!("Mapping VGA buffer.");
        let vga_buffer = Frame(crate::vga_text::VGA_BUFFER_ADDRESS);
        tlb_flush.consume(m.map_kernel_space(&vga_buffer, EntryFlags::WRITABLE));
        forget(vga_buffer);

        debug!("Mapping Multiboot info structures.");
        map_region_in_kernel(
            m,
            PhysicalAddress::new(multiboot_info.start_address() - *KERNEL_BASE),
            (multiboot_info.end_address() - multiboot_info.start_address()) as usize,
            EntryFlags::PRESENT,
        );

        tlb_flush.consume_other(closure_tlb_flush);
    });

    debug!("Finished remapping.");
    let old = top.switch(new_table);
    debug!("Switched to new table.");

    //    PHYSICAL_ALLOCATOR.free(old.frame());
    let guard = KERNEL_BASE + *old.address();
    tlb_flush.consume(top.unmap(guard));
    debug!("Guard page at {}", KERNEL_BASE + *old.address());
    forget(old.into_frame());

    tlb_flush.flush();
}
