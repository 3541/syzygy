mod rsdt;

use core::slice;
use core::str;

use logc::{debug, error, warn};
use multiboot2::{RsdpV1Tag, RsdpV2Tag};

use crate::memory::paging::EntryFlags;
use crate::memory::region::TypedRegion;
use crate::memory::{Address, PhysicalAddress};
use crate::task::Task;
use rsdt::{AcpiRootTable, Rsdt, Xsdt};

static mut ROOT_TABLE: Option<AcpiRootTable> = None;

trait AcpiTable {
    const SIGNATURE: &'static str;

    fn new(address: PhysicalAddress) -> Option<TypedRegion<Self>>;
    fn header(&self) -> &SdtHeader;

    fn is_valid(&self) -> bool {
        self.header().is_valid(Self::SIGNATURE)
    }
}

#[repr(C, packed)]
struct SdtHeader {
    signature: [u8; 4],
    length: u32,
    revision: u8,
    checksum: u8,
    oem_id: [u8; 6],
    oem_table_id: [u8; 8],
    oem_revision: u32,
    creator_id: u32,
    creator_revision: u32,
}

impl SdtHeader {
    unsafe fn new(address: PhysicalAddress) -> Option<TypedRegion<SdtHeader>> {
        let allocator = Task::current().lock().kernel_allocator();

        allocator.make_mapped(address, EntryFlags::PRESENT)
    }

    fn is_valid(&self, signature: &str) -> bool {
        if str::from_utf8(&self.signature) != Ok(signature) {
            return false;
        }

        let sum =
            unsafe { slice::from_raw_parts(self as *const _ as *const u8, self.length as usize) }
                .iter()
                .fold(0, |s: u8, b| s.wrapping_add(*b));
        debug!("Checksum is {}", sum);
        sum == 0
    }
}

pub fn init(rsdp_v1: Option<&RsdpV1Tag>, rsdp_v2: Option<&RsdpV2Tag>) {
    if let Some(rsdp_v2) = rsdp_v2 {
        debug!("Got RSDPv2.");

        let signature = rsdp_v2.signature().unwrap();
        let xsdt_address = PhysicalAddress::new(rsdp_v2.xsdt_address());

        debug!("{:x?}", rsdp_v2);

        if !rsdp_v2.checksum_is_valid() || signature != "RSD PTR " {
            error!("RSDP invalid!");
            error!("\tChecksum: {}", rsdp_v2.checksum_is_valid());
            error!("\tSignature: {}", rsdp_v2.signature().unwrap_or("(none)"));
            return;
        } else {
            debug!("\tValid.");
        }

        unsafe {
            ROOT_TABLE = Some(AcpiRootTable::Xsdt(
                Xsdt::new(xsdt_address).expect("Invalid XSDT."),
            ))
        }
    } else if let Some(rsdp_v1) = rsdp_v1 {
        debug!("Got RSDPv1.");

        let signature = rsdp_v1.signature().unwrap();
        let rsdt_address = PhysicalAddress::new(rsdp_v1.rsdt_address());

        debug!("{:x?}", rsdp_v1);

        if !rsdp_v1.checksum_is_valid() || signature != "RSD PTR " {
            error!("RSDP invalid!");
            error!("\tChecksum: {}", rsdp_v1.checksum_is_valid());
            error!("\tSignature: {}", rsdp_v1.signature().unwrap_or("(none)"));
            return;
        } else {
            debug!("\tValid.");
        }

        unsafe {
            ROOT_TABLE = Some(AcpiRootTable::Rsdt(
                Rsdt::new(rsdt_address).expect("Invalid RSDT."),
            ))
        }
    } else {
        debug!("No RSDP");
        warn!("This is currently not handled.");
    }
}
