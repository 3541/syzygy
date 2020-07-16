use core::mem::size_of;

use logc::debug;

use super::{AcpiTable, SdtHeader};
use crate::memory::region::TypedRegion;
use crate::memory::{PhysicalAddress, VirtualAddress};

pub enum AcpiRootTable {
    Rsdt(TypedRegion<Rsdt>),
    Xsdt(TypedRegion<Xsdt>),
}

trait RootTable: AcpiTable {
    const POINTER_SIZE: usize;

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
                (length - size_of::<SdtHeader>()) / Self::POINTER_SIZE,
            )
        };

        if !ret.is_valid() {
            None
        } else {
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
    const POINTER_SIZE: usize = size_of::<u32>();
}

impl AcpiTable for Rsdt {
    const SIGNATURE: &'static str = "RSDT";

    fn new(address: PhysicalAddress) -> Option<TypedRegion<Rsdt>> {
        let header = unsafe { SdtHeader::new(address).expect("Unable to map RSDT header.") };

        Rsdt::from_header(header)
    }

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

#[repr(C, packed)]
pub struct Xsdt {
    header: SdtHeader,
    pointers: [VirtualAddress],
}

impl RootTable for Xsdt {
    const POINTER_SIZE: usize = size_of::<VirtualAddress>();
}

impl AcpiTable for Xsdt {
    const SIGNATURE: &'static str = "XSDT";

    fn new(address: PhysicalAddress) -> Option<TypedRegion<Xsdt>> {
        let header = unsafe { SdtHeader::new(address).expect("Unable to map XSDT header.") };

        Xsdt::from_header(header)
    }

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}
