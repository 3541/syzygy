use core::mem::size_of;

use logc::debug;

use super::{AcpiTable, SdtHeader};
use crate::memory::region::TypedRegion;
use crate::PhysicalAddress;

#[repr(C, packed)]
pub struct Hpet {
    header: SdtHeader,
    hardware_rev_id: u8,
    flags: u8,
    pci_vendor_id: u16,
    address_space_id: u8,
    register_bit_width: u8,
    register_bit_offset: u8,
    _reserved: u8,
    address: PhysicalAddress,
    number: u8,
    minimum_tick: u16,
    page_protection: u8,
}

impl AcpiTable for Hpet {
    const SIGNATURE: &'static str = "HPET";

    fn new(header: TypedRegion<SdtHeader>) -> Option<TypedRegion<Hpet>> {
        assert_eq!(header.length as usize, size_of::<Hpet>());

        let offset = header.offset();
        let region = header.into_region().grow(size_of::<Hpet>());

        let ret = region.into_typed::<Hpet>(offset);

        if !ret.is_valid() {
            None
        } else {
            debug!("\t* HPET");
            Some(ret)
        }
    }

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}
