use alloc::boxed::Box;
use alloc::vec::Vec;
use core::convert::TryInto;
use core::fmt::Debug;
use core::mem::size_of;

use logc::debug;

use super::{AcpiTable, SdtHeader};
use crate::memory::region::TypedRegion;
use crate::memory::{Address, PhysicalAddress};

pub enum AcpiRootTable {
    Rsdt(TypedRegion<Rsdt>),
    Xsdt(TypedRegion<Xsdt>),
    Empty,
}

impl AcpiRootTable {
    pub(super) fn pointers(&self) -> Box<dyn Iterator<Item = PhysicalAddress> + '_> {
        match self {
            Self::Rsdt(r) => r.pointers(),
            Self::Xsdt(x) => x.pointers(),
            Self::Empty => panic!("Invalid."),
        }
    }
}

impl Default for AcpiRootTable {
    fn default() -> AcpiRootTable {
        AcpiRootTable::Empty
    }
}

trait RootTable: AcpiTable {
    type Pointer: TryInto<usize> + Copy;

    fn n_pointers(&self) -> usize {
        (self.header().length as usize - size_of::<SdtHeader>()) / size_of::<Self::Pointer>()
    }

    // Rust pls. This is necessary because taking references to unaligned memory
    // is undefined behavior.
    fn pointers(&self) -> Box<dyn Iterator<Item = PhysicalAddress> + '_>
    where
        <Self::Pointer as TryInto<usize>>::Error: Debug,
    {
        let mut pointers: Vec<Self::Pointer> = Vec::with_capacity(self.n_pointers());
        unsafe {
            ((self as *const _ as *const () as usize + size_of::<SdtHeader>())
                as *const Self::Pointer)
                .copy_to_nonoverlapping(pointers.as_mut_ptr(), self.n_pointers());
            pointers.set_len(self.n_pointers());
        }
        Box::new(
            pointers
                .into_iter()
                .map(|a| PhysicalAddress::new(a.try_into().unwrap())),
        )
    }

    fn from_header(header: TypedRegion<SdtHeader>) -> Option<TypedRegion<Self>> {
        let length = header.length as usize;
        let offset = header.offset();
        let region = header.into_region().grow(length);

        // Note: this is evil, and depends on internal fat pointer
        // details. Namely: The second field of a fat pointer to a DST
        // is the length of the dynamically-sized field.
        let ret: TypedRegion<Self> = unsafe {
            region.into_typed_unsized(
                offset,
                (length - size_of::<SdtHeader>()) / size_of::<Self::Pointer>(),
            )
        };

        if !ret.is_valid() {
            None
        } else {
            debug!("\t* (R/X)SDT");
            Some(ret)
        }
    }
}

#[repr(C, packed)]
pub struct Rsdt {
    header: SdtHeader,
    pointers: [u32],
}

impl RootTable for Rsdt {
    type Pointer = u32;
}

impl AcpiTable for Rsdt {
    const SIGNATURE: &'static str = "RSDT";

    fn new(header: TypedRegion<SdtHeader>) -> Option<TypedRegion<Rsdt>> {
        Rsdt::from_header(header)
    }

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

#[repr(C, packed)]
pub struct Xsdt {
    header: SdtHeader,
    pointers: [PhysicalAddress],
}

impl RootTable for Xsdt {
    type Pointer = PhysicalAddress;
}

impl AcpiTable for Xsdt {
    const SIGNATURE: &'static str = "XSDT";

    fn new(header: TypedRegion<SdtHeader>) -> Option<TypedRegion<Xsdt>> {
        Xsdt::from_header(header)
    }

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}
