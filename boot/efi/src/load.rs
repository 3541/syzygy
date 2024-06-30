use core::fmt::{self, Write};

use elf::{
    abi,
    dynamic::DynamicTable,
    endian::NativeEndian,
    file::{Class, FileHeader},
    symbol::Symbol,
    to_str, ElfBytes,
};
use r_efi::efi::BootServices;
use rand::{rngs::OsRng, Rng};

use crate::{
    log::Log,
    uefi::{Image, Pages, EFI_PAGE_SIZE},
    Result,
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
            }
            Self::InvalidMachine(m) => {
                write!(
                    f,
                    "Invalid machine {}.",
                    to_str::e_machine_to_str(*m).unwrap_or("<unknown>")
                )
            }
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
    if h.e_type != abi::ET_DYN {
        return Err(Error::InvalidType(h.e_type).into());
    }

    Ok(())
}

fn relocate(
    _log: &mut Log,
    dy: &DynamicTable<'_, NativeEndian>,
    _data: &[u8],
    _offset: usize,
) -> Result<()> {
    for d in dy.iter() {
        match d.d_tag {
            abi::DT_REL | abi::DT_RELA | abi::DT_RELAENT | abi::DT_RELASZ | abi::DT_JMPREL => {
                todo!("relocations")
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn load_image(log: &mut Log, bs: &BootServices, image: &Image) -> Result<()> {
    let file = ElfBytes::<NativeEndian>::minimal_parse(image.data())?;
    validate(&file.ehdr)?;

    let phdrs = file
        .segments()
        .ok_or(Error::InvalidStructure("No program headers"))?;
    let dy = file
        .dynamic()?
        .ok_or(Error::InvalidStructure("No PT_DYNAMIC program header"))?;

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
    let max = nonempty().map(|h| h.p_vaddr).max().unwrap();

    assert!(align <= EFI_PAGE_SIZE as u64);
    assert!(min < max);

    let size = (max - min) as usize;
    writeln!(log, "Loading ELF image, {} bytes.\r", size)?;

    let mut dst = Pages::new(bs, size)?;
    let mut r = OsRng;
    let rel_offset = r.gen_range(0..=(usize::MAX - dst.size())) & !(EFI_PAGE_SIZE - 1);
    writeln!(log, "Offset: {:#x}.\r", rel_offset)?;

    for h in nonempty() {
        assert!(h.p_memsz >= h.p_filesz);
        assert!(h.p_vaddr >= min);

        let offset = (h.p_vaddr - min) as usize;
        writeln!(
            log,
            "Loading {}{}{}: {:#x}@P{:#x}.\r",
            if h.p_flags & abi::PF_R != 0 { "R" } else { " " },
            if h.p_flags & abi::PF_W != 0 { "W" } else { " " },
            if h.p_flags & abi::PF_X != 0 { "X" } else { " " },
            h.p_memsz,
            dst.ptr() as usize + offset,
        )?;

        let region = &mut dst.data()[offset..offset + h.p_memsz as usize];
        region[..h.p_filesz as usize].copy_from_slice(file.segment_data(&h)?);

        relocate(log, &dy, region, rel_offset)?;
    }

    todo!()
}
