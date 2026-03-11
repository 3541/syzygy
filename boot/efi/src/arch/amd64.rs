/*
 * Copyright (c) 2024, 2026 Alex O'Brien <3541@3541.website>
 *
 * This file is part of Syzygy.
 *
 * Syzygy is free software: you can redistribute it and/or modify it under the
 * terms of version 3 the GNU General Public License as published by the Free
 * Software Foundation.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software. If not, see <https://www.gnu.org/licenses/>.
 */

use core::{
    arch::asm,
    fmt::Write,
    mem::{MaybeUninit, size_of},
    ptr, slice,
};

use bitflags::bitflags;
use elf::abi;
use r_efi::efi::BootServices;

use crate::{Result, load::Image, log::Log, uefi::Pages};
use common::constants::{MB, PT_RECURSIVE_INDEX};

const PAGE_SIZE: usize = 0x1000;
const ENTRY_MASK: u64 = 0x000F_FFFF_FFFF_F000;
const OFFSET_BITS: usize = 12;
const INDEX_BITS: usize = 9;
const INDEX_MASK: u64 = (1 << INDEX_BITS) - 1;

bitflags! {
    #[derive(Clone, Copy)]
    struct EntryFlags: u64 {
        const PRESENT = 1;
        const WRITABLE = 1 << 1;
        const USER = 1 << 2;
        const NO_EXEC = 1 << 63;
    }
}

fn entry<T>(ptr: *const T, flags: EntryFlags) -> u64 {
    let address = ptr as u64;
    assert_eq!(address & !ENTRY_MASK, 0, "Invalid address P{address:#x}.");

    address | (flags | EntryFlags::PRESENT).bits()
}

fn chunks<T, const N: usize>(data: &mut [u8]) -> [&mut [T]; N] {
    assert_eq!(data.len() % N, 0);
    assert_eq!(data.len() / N % size_of::<T>(), 0);

    let mut res = [const { MaybeUninit::<&mut [T]>::uninit() }; N];
    for (i, chunk) in data.chunks_mut(data.len() / N).enumerate() {
        res[i].write(unsafe {
            slice::from_raw_parts_mut(chunk.as_mut_ptr() as *mut T, chunk.len() / size_of::<T>())
        });
    }

    unsafe { MaybeUninit::array_assume_init(res) }
}

fn index<T>(ptr: *const T, level: usize) -> usize {
    ((ptr as u64 >> (OFFSET_BITS + INDEX_BITS * (level - 1))) & INDEX_MASK) as usize
}

fn flags(elf: u32) -> EntryFlags {
    assert!(elf & abi::PF_W == 0 || elf & abi::PF_X == 0);

    (if elf & abi::PF_W != 0 {
        EntryFlags::WRITABLE
    } else {
        EntryFlags::empty()
    }) | (if elf & abi::PF_X != 0 {
        EntryFlags::empty()
    } else {
        EntryFlags::NO_EXEC
    })
}

unsafe fn current_pml4() -> &'static [u64] {
    let mut pml4: *mut [u64; 512] = ptr::null_mut();
    unsafe {
        asm!("mov {}, cr3", out(reg) pml4, options(nomem, nostack));
        &mut *pml4
    }
}

fn load_pml4(addr: usize) {
    unsafe { asm!("mov cr3, {}", in(reg) addr, options(nomem, nostack)) }
}

pub fn map_image(log: &mut Log, bs: &BootServices, image: &Image) -> Result<()> {
    assert!(
        image.data.len() <= 2 * MB,
        "Quick and dirty bootstrap mapping assumes kernel <= 2 MB. Image is {:#x} bytes.",
        image.data.len()
    );

    let current_pml4 = unsafe { current_pml4() };
    writeln!(
        log,
        "Creating bootstrap page tables (UEFI PML4: P{:#x}).",
        current_pml4.as_ptr() as usize
    )?;

    let mut tables = Pages::new_count(bs, 4)?;
    tables.data().fill(0);

    let [pml4, pdp, pd, pt] = chunks::<u64, 4>(tables.data());
    assert_eq!(pml4.len(), 512);
    writeln!(log, "New PML4: P{:#x}", pml4.as_ptr() as usize)?;

    pml4.copy_from_slice(current_pml4);

    let table_flags = EntryFlags::WRITABLE;

    assert_eq!(image.base % PAGE_SIZE, 0);
    let virt_base = image.base as *const u8;

    let pml4_pdp_index = index(virt_base, 4);
    assert_eq!(pml4[PT_RECURSIVE_INDEX], 0);
    assert_eq!(pml4[pml4_pdp_index], 0);
    assert_ne!(PT_RECURSIVE_INDEX, pml4_pdp_index);
    pml4[PT_RECURSIVE_INDEX] = entry(pml4.as_ptr(), table_flags);
    pml4[index(virt_base, 4)] = entry(pdp.as_ptr(), table_flags);
    pdp[index(virt_base, 3)] = entry(pd.as_ptr(), table_flags);

    let pd_index = index(virt_base, 2);
    pd[pd_index] = entry(pt.as_ptr(), table_flags);

    let mut count = 0;
    for region in &image.map {
        let real_base = region.base - image.base + image.data.ptr() as usize;
        let base = real_base & !(PAGE_SIZE - 1);
        let align_offset = real_base - base;
        let page_count = (region.size + align_offset + PAGE_SIZE - 1) / PAGE_SIZE;

        assert_eq!(
            base % PAGE_SIZE,
            0,
            "Region P{base:#x} not aligned to page size."
        );

        for i in 0..page_count {
            let offset = i * PAGE_SIZE;
            let virt = (region.base + offset - align_offset) as *const u8;
            let phys = (base + offset) as *const u8;
            writeln!(
                log,
                "Mapping {}{}{} V{:#x} => P{:#x}, {}:{}:{}:{}",
                if region.flags & abi::PF_R != 0 {
                    "R"
                } else {
                    " "
                },
                if region.flags & abi::PF_W != 0 {
                    "W"
                } else {
                    " "
                },
                if region.flags & abi::PF_X != 0 {
                    "X"
                } else {
                    " "
                },
                virt as usize,
                phys as usize,
                index(virt, 4),
                index(virt, 3),
                index(virt, 2),
                index(virt, 1),
            )?;

            assert_eq!(
                index(virt, 2),
                pd_index,
                "Mapping spans multiple PDs (V{:#x} P{:#x}).",
                virt as usize,
                phys as usize
            );

            let index = index(virt, 1);
            let entry = entry(phys, flags(region.flags));
            assert!(
                pt[index] == 0 || pt[index] == entry,
                "Mapping at {} (V{:#x}) already occupied with distinct flags.",
                index,
                virt as usize
            );

            pt[index] = entry;
            count += 1;
        }
    }

    load_pml4(pml4.as_ptr() as usize);
    tables.leak();

    writeln!(log, "Mapped {count} pages in 1 page table.")?;

    Ok(())
}
