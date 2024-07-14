/*
 * Copyright (c) 2024 Alex O'Brien <3541@3541.website>
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
    fmt::Write,
    mem::{size_of, MaybeUninit},
    slice,
};

use bitflags::bitflags;
use elf::abi;
use r_efi::efi::BootServices;
use util::constants::MB;

use crate::{load::Image, log::Log, uefi::Pages, Result};

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
    unsafe {
        writeln!(
            crate::log::LOG.unwrap(),
            "Address is {address:#x}, index is {}\r",
            index(ptr, 1)
        )
    };
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
    assert_ne!(elf & abi::PF_R, 0);

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

pub fn map_image(log: &mut Log, bs: &BootServices, image: &Image) -> Result<()> {
    assert!(
        image.data.len() <= 2 * MB,
        "Quick and dirty bootstrap mapping assumes kernel <= 2 MB. Image is {:#x} bytes.",
        image.data.len()
    );

    let mut tables = Pages::new_count(bs, 4)?;
    tables.data().fill(0);

    let [pml4, pdp, pd, pt] = chunks::<u64, 4>(tables.data());
    assert_eq!(pml4.len(), 512);

    let table_flags = EntryFlags::WRITABLE | EntryFlags::NO_EXEC;

    assert_eq!(image.base % PAGE_SIZE, 0);
    let virt_base = image.base as *const u8;

    pml4[pml4.len() - 1] = entry(pml4.as_ptr(), table_flags);
    pml4[index(virt_base, 4)] = entry(pdp.as_ptr(), table_flags);
    pdp[index(virt_base, 3)] = entry(pd.as_ptr(), table_flags);

    let pd_index = index(virt_base, 3);
    pd[pd_index] = entry(pt.as_ptr(), table_flags);

    let mut count = 0;
    for region in &image.map {
        let real_base = region.base - image.base + image.data.ptr() as usize;
        let base = real_base & !(PAGE_SIZE - 1);
        let align_offset = real_base - base;

        assert_eq!(
            base % PAGE_SIZE,
            0,
            "Region P{base:#x} not aligned to page size."
        );

        for i in 0..=(region.size - 1 + align_offset) / PAGE_SIZE + 1 {
            let offset = i * PAGE_SIZE;
            let virt = (region.base + offset - align_offset) as *const u8;
            let phys = (base + offset) as *const u8;
            assert_eq!(
                index(virt, 2),
                pd_index,
                "Mapping spans multiple PDs (P{:#x}).",
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

    writeln!(log, "Mapped {count} pages in 1 page table.\r")?;

    Ok(())
}
