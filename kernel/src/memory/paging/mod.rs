pub mod mapper;
pub mod table;
mod temp_page;

use alloc::sync::Arc;
use core::mem::{forget, replace};

use logc::debug;
use multiboot2::ElfSectionIter;
use spin::{Mutex, MutexGuard};

pub use table::{ActiveTopLevelTable, EntryFlags, TopLevelTable};
pub use temp_page::TempPage;

use super::region::VirtualRegionAllocator;
use super::{Address, Frame, PhysicalAddress, PhysicalMemory, VirtualAddress, PHYSICAL_ALLOCATOR};
use crate::constants::KERNEL_BASE;
use mapper::{Mapper, TLBFlush};
use table::InactiveTopLevelTable;

pub struct Pager {
    table: TopLevelTable,
    kernel_allocator: Arc<VirtualRegionAllocator>,
}

impl Pager {
    pub fn new(&self) -> Pager {
        // The kernel virtual allocator is shared between all tasks.
        let kernel_allocator = self.kernel_allocator.clone();
        if let TopLevelTable::Active(current_table) = &self.table {
            let new_table = current_table.lock().clone_kernel_mappings();

            Pager {
                table: TopLevelTable::Inactive(Mutex::new(new_table)),
                kernel_allocator,
            }
        } else {
            panic!("Tried to create new pager from inactive pager.");
        }
    }

    pub fn create_existing(
        table: TopLevelTable,
        kernel_allocator: Arc<VirtualRegionAllocator>,
    ) -> Pager {
        Pager {
            table,
            kernel_allocator,
        }
    }

    pub fn kernel_allocator(&self) -> &Arc<VirtualRegionAllocator> {
        &self.kernel_allocator
    }

    pub fn page_table(&mut self) -> &mut TopLevelTable {
        &mut self.table
    }

    pub fn active_table(&self) -> MutexGuard<ActiveTopLevelTable> {
        match &self.table {
            TopLevelTable::Active(t) => t.lock(),
            _ => panic!("Table not active."),
        }
    }

    pub fn inactive_table(&self) -> MutexGuard<InactiveTopLevelTable> {
        match &self.table {
            TopLevelTable::Inactive(t) => t.lock(),
            _ => panic!("Table not inactive."),
        }
    }

    pub fn take_inactive_table(&mut self) -> InactiveTopLevelTable {
        match replace(&mut self.table, TopLevelTable::None) {
            TopLevelTable::Inactive(table) => table.into_inner(),
            TopLevelTable::Active(_) => panic!("Tried to take from an active table."),
            TopLevelTable::None => panic!("Tried to take a TopLevelTable::None."),
        }
    }

    pub fn take_active_table(&mut self) -> ActiveTopLevelTable {
        match replace(&mut self.table, TopLevelTable::None) {
            TopLevelTable::Active(table) => table.into_inner(),
            TopLevelTable::Inactive(_) => panic!("Tried to take from an inactive table."),
            TopLevelTable::None => panic!("Tried to take a TopLevelTable::None."),
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

            debug!(
                "Mapping section 0x{:x}-0x{:x} (0x{:x}).",
                section.start_address(),
                section.end_address(),
                section.size()
            );

            map_region_in_kernel(
                m,
                PhysicalAddress::new((section.start_address() as usize) - *KERNEL_BASE),
                (section.end_address() - section.start_address()) as usize,
                EntryFlags::from_elf(&section) | EntryFlags::GLOBAL,
            );
        }

        debug!("Mapping VGA buffer.");
        let vga_buffer = Frame(crate::vga_text::VGA_BUFFER_ADDRESS);
        tlb_flush.consume(m.map_kernel_space(
            &vga_buffer,
            EntryFlags::WRITABLE | EntryFlags::GLOBAL | EntryFlags::NO_EXECUTE,
        ));
        forget(vga_buffer);

        debug!("Mapping Multiboot info structures.");
        map_region_in_kernel(
            m,
            PhysicalAddress::new(multiboot_info.start_address() - *KERNEL_BASE),
            (multiboot_info.end_address() - multiboot_info.start_address()) as usize,
            EntryFlags::PRESENT | EntryFlags::GLOBAL | EntryFlags::NO_EXECUTE,
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
