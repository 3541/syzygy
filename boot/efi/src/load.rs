use core::fmt;

use elf::{
    endian::NativeEndian,
    file::{Class, FileHeader},
    ElfBytes,
};

use crate::{uefi::Image, Result};

pub enum Error {
    InvalidClass(Class),
    InvalidMachine(u16),
    InvalidType(u16),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidClass(c) => {
                write!(f, "Invalid class {:?}, expected {:?}.", c, Class::ELF64)
            }
            Self::InvalidMachine(m) => write!(f, "Invalid machine {}.", m),
            Self::InvalidType(t) => write!(f, "Invalid type {}.", t),
        }
    }
}

fn validate(h: &FileHeader<NativeEndian>) -> Result<()> {
    if h.class != Class::ELF64 {
        return Err(Error::InvalidClass(h.class).into());
    }
    if h.e_machine != elf::abi::EM_X86_64 {
        return Err(Error::InvalidMachine(h.e_machine).into());
    }
    if h.e_type != elf::abi::ET_REL {
        return Err(Error::InvalidType(h.e_type).into());
    }
    Ok(())
}

pub fn load_image(image: &Image) -> Result<()> {
    let file = ElfBytes::<NativeEndian>::minimal_parse(image.data())?;
    validate(&file.ehdr)?;

    todo!()
}
