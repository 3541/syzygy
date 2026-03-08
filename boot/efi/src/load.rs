/*
 * LOAD: ELF loader.
 *
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
    fmt::{self, Write},
    mem::{size_of, transmute},
};

use arrayvec::ArrayVec;
use elf::{
    ElfBytes, abi,
    endian::NativeEndian,
    file::{Class, FileHeader},
    relocation::RelaIterator,
    to_str,
};
use r_efi::efi::BootServices;
use rand::{RngExt, rand_core::UnwrapErr, rngs::SysRng};

use crate::{
    Result,
    log::Log,
    uefi::{EFI_PAGE_SIZE, FileImage, Pages},
};
use common::{
    abi::{ENTRYPOINT, Entrypoint},
    constants,
};

pub enum Error {
    InvalidClass(Class),
    InvalidMachine(u16),
    InvalidType(u16),
    InvalidStructure(&'static str),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidClass(c) => {
                write!(f, "Invalid class {:?}, expected {:?}.", c, Class::ELF64)
            },
            Self::InvalidMachine(m) => {
                write!(
                    f,
                    "Invalid machine {}.",
                    to_str::e_machine_to_str(*m).unwrap_or("<unknown>")
                )
            },
            Self::InvalidType(t) => write!(
                f,
                "Invalid type {}.",
                to_str::e_type_to_str(*t).unwrap_or("<unknown>")
            ),
            Self::InvalidStructure(m) => write!(f, "Invalid ELF structure: {}.", m),
        }
    }
}

fn validate(h: &FileHeader<NativeEndian>) -> Result<()> {
    if h.class != Class::ELF64 {
        return Err(Error::InvalidClass(h.class).into());
    }
    if h.e_machine != abi::EM_X86_64 {
        return Err(Error::InvalidMachine(h.e_machine).into());
    }
    if h.e_type != abi::ET_DYN && h.e_type != abi::ET_EXEC {
        return Err(Error::InvalidType(h.e_type).into());
    }

    Ok(())
}

fn relocate(
    log: &mut Log,
    file: &ElfBytes<NativeEndian>,
    data: &mut [u8],
    file_base: usize,
    real_base: usize,
) -> Result<()> {
    let dy = file
        .dynamic()?
        .ok_or(Error::InvalidStructure("No PT_DYNAMIC program header"))?;

    let find_only = |t| {
        let mut it = dy.iter().filter(|d| d.d_tag == t);
        let res = it.next();
        if it.next().is_some() {
            Err(crate::Error::from(Error::InvalidStructure(
                "Multiple relocation headers of single type",
            )))
        } else {
            Ok(res)
        }
    };

    let relas = find_only(abi::DT_RELA)?;
    if let Some(relas) = relas {
        let relasz = find_only(abi::DT_RELASZ)?
            .ok_or(Error::InvalidStructure(
                "DT_RELA present, but missing DT_RELSZ",
            ))?
            .d_val();
        let relaent = find_only(abi::DT_RELAENT)?
            .ok_or(Error::InvalidStructure(
                "DT_RELA present, but missing DT_RELAENT",
            ))?
            .d_val();

        if let Some(relacount) = find_only(abi::DT_RELACOUNT)? {
            if relasz / relaent != relacount.d_val() {
                return Err(Error::InvalidStructure(
                    "DT_RELACOUNT does not match DT_RELASZ / DT_RELAENT",
                )
                .into());
            }
        }

        let rel_offset = relas.d_ptr() as usize - file_base;
        for rela in RelaIterator::new(
            NativeEndian,
            file.ehdr.class,
            &data[rel_offset..rel_offset + relasz as usize],
        ) {
            let res = match rela.r_type {
                abi::R_X86_64_RELATIVE => real_base as i64 + rela.r_addend,
                _ => {
                    todo!("Unhandled relocation {}", rela.r_type)
                },
            } as u64;

            let i = rela.r_offset as usize - file_base;
            assert!(i + size_of::<u64>() <= rel_offset || rel_offset + relasz as usize <= i);
            assert!(i + size_of::<u64>() <= data.len());
            // SAFETY: Previous assertion verifies this does not overlap with subslice being iterated.
            unsafe { (&data[i] as *const _ as *mut u64).write(res) };
        }
    }

    for d in dy.iter() {
        match d.d_tag {
            abi::DT_REL | abi::DT_RELENT | abi::DT_RELSZ | abi::DT_JMPREL => {
                todo!(
                    "{} relocations",
                    to_str::d_tag_to_str(d.d_tag).unwrap_or("<unknown>")
                )
            },
            _ => {},
        }
    }

    Ok(())
}

fn find_entrypoint(file: &ElfBytes<NativeEndian>) -> Result<usize> {
    let (symtab, strtab) = file
        .symbol_table()?
        .ok_or(Error::InvalidStructure("Unable to find symbol table."))?;
    symtab
        .iter()
        .filter(|e| strtab.get(e.st_name as usize).ok() == Some(ENTRYPOINT))
        .map(|s| s.st_value as usize)
        .next()
        .ok_or(Error::InvalidStructure("Failed to find entrypoint in symbol table.").into())
}

pub struct Region {
    pub base: usize,
    pub size: usize,
    pub flags: u32,
}

pub struct Image {
    pub data: Pages,
    pub base: usize,
    pub entrypoint: Entrypoint,
    pub map: ArrayVec<Region, 32>,
}

impl Image {
    pub fn load(log: &mut Log, bs: &BootServices, image: &FileImage) -> Result<Self> {
        let file = ElfBytes::<NativeEndian>::minimal_parse(image.data())?;
        validate(&file.ehdr)?;

        let phdrs = file
            .segments()
            .ok_or(Error::InvalidStructure("No program headers"))?;

        let nonempty = || {
            phdrs
                .iter()
                .filter(|h| h.p_type == abi::PT_LOAD && h.p_memsz != 0)
        };
        let align = nonempty()
            .map(|h| h.p_align)
            .max()
            .ok_or(Error::InvalidStructure("No nonempty program headers"))?;
        let min = nonempty().map(|h| h.p_vaddr).min().unwrap();
        let max = nonempty().map(|h| h.p_vaddr + h.p_memsz).max().unwrap();

        assert!(align <= EFI_PAGE_SIZE as u64);
        assert!(min < max);

        let size = (max - min) as usize;
        writeln!(log, "Loading ELF image, {} bytes.", size)?;

        let mut dst = Pages::new(bs, size)?;

        let mut r = UnwrapErr(SysRng);
        let load_address = r.random_range(0xFFFFFFFF80100000..=(usize::MAX - dst.len()))
            & !(2 * constants::MB - 1);

        let mut map = ArrayVec::<Region, 32>::new();
        for h in nonempty() {
            assert!(h.p_memsz >= h.p_filesz);
            assert!(h.p_vaddr >= min);

            let offset = (h.p_vaddr - min) as usize;
            writeln!(
                log,
                "Loading {}{}{}: P{:#x} - P{:#x}.",
                if h.p_flags & abi::PF_R != 0 { "R" } else { " " },
                if h.p_flags & abi::PF_W != 0 { "W" } else { " " },
                if h.p_flags & abi::PF_X != 0 { "X" } else { " " },
                dst.ptr() as usize + offset,
                dst.ptr() as usize + offset + h.p_memsz as usize,
            )?;

            let region = &mut dst.data()[offset..offset + h.p_memsz as usize];
            region[..h.p_filesz as usize].copy_from_slice(file.segment_data(&h)?);

            if h.p_memsz > h.p_filesz {
                region[h.p_filesz as usize..h.p_memsz as usize].fill(0);
            }

            map.push(Region {
                base: load_address + offset,
                size: h.p_memsz as usize,
                flags: h.p_flags,
            });
        }

        let entrypoint = find_entrypoint(&file)? - min as usize + load_address;
        writeln!(log, "Found entrypoint at {entrypoint:#x}.");

        writeln!(log, "Relocating: V{min:#x} to V{load_address:#x}.")?;
        relocate(log, &file, dst.data(), min as usize, load_address)?;

        Ok(Self {
            data: dst,
            base: load_address,
            entrypoint: unsafe { transmute(entrypoint as *const ()) },
            map,
        })
    }

    pub fn leak(self) -> (ArrayVec<Region, 32>, &'static mut [u8], Entrypoint) {
        let Self { data, map, entrypoint, .. } = self;
        (map, data.leak(), entrypoint)
    }
}
