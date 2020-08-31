mod hpet;
mod rsdt;

use core::slice;
use core::str;

use logc::{debug, error, warn};
use multiboot2::{RsdpV1Tag, RsdpV2Tag};

use crate::memory::paging::EntryFlags;
use crate::memory::region::TypedRegion;
use crate::memory::{Address, PhysicalAddress};
use crate::task::Task;
use hpet::Hpet;
use rsdt::{AcpiRootTable, Rsdt, Xsdt};

trait AcpiTable {
    const SIGNATURE: &'static str;

    fn new(header: TypedRegion<SdtHeader>) -> Option<TypedRegion<Self>>;
    fn header(&self) -> &SdtHeader;

    fn is_valid(&self) -> bool {
        self.header().is_valid(Self::SIGNATURE)
    }

    fn signature(&self) -> &'static str {
        return Self::SIGNATURE;
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
        sum == 0
    }
}

#[derive(Default)]
pub struct Acpi {
    root: AcpiRootTable,
    hpet: Option<TypedRegion<Hpet>>,
}

static mut ACPI: Option<Acpi> = None;

impl Acpi {
    /// # Safety
    /// This is safe so long as it is only called after acpi::init
    pub fn the() -> Option<&'static Acpi> {
        unsafe { ACPI.as_ref() }
    }

    pub fn init(rsdp_v1: Option<&RsdpV1Tag>, rsdp_v2: Option<&RsdpV2Tag>) {
        if let Some(rsdp_v2) = rsdp_v2 {
            let signature = rsdp_v2.signature().unwrap();
            let xsdt_address = PhysicalAddress::new(rsdp_v2.xsdt_address());

            debug!("Got RSDPv2 with XSDT at {}.", xsdt_address);

            if !rsdp_v2.checksum_is_valid() || signature != "RSD PTR " {
                error!("RSDP invalid!");
                error!("\tChecksum: {}", rsdp_v2.checksum_is_valid());
                error!("\tSignature: {}", rsdp_v2.signature().unwrap_or("(none)"));
                return;
            } else {
                debug!("\tValid.");
            }

            unsafe {
                ACPI = Some(Acpi {
                    root: AcpiRootTable::Xsdt(
                        Xsdt::new(
                            SdtHeader::new(xsdt_address).expect("Unable to map XSDT header."),
                        )
                        .expect("Invalid XSDT."),
                    ),
                    ..Default::default()
                });
            }
        } else if let Some(rsdp_v1) = rsdp_v1 {
            let signature = rsdp_v1.signature().unwrap();
            let rsdt_address = PhysicalAddress::new(rsdp_v1.rsdt_address());
            debug!("Got RSDPv1 with RSDT at {}.", rsdt_address);

            if !rsdp_v1.checksum_is_valid() || signature != "RSD PTR " {
                error!("RSDP invalid!");
                error!("\tChecksum: {}", rsdp_v1.checksum_is_valid());
                error!("\tSignature: {}", rsdp_v1.signature().unwrap_or("(none)"));
                return;
            } else {
                debug!("\tValid.");
            }

            unsafe {
                ACPI = Some(Acpi {
                    root: AcpiRootTable::Rsdt(
                        Rsdt::new(
                            SdtHeader::new(rsdt_address).expect("Unable to map RSDT header."),
                        )
                        .expect("Invalid RSDT."),
                    ),
                    ..Default::default()
                });
            }
        } else {
            debug!("No RSDP");
            warn!("This is currently not handled.");
        }

        let mut acpi = unsafe { ACPI.as_mut().unwrap() };
        for pointer in acpi.root.pointers() {
            if let Some(header) = unsafe { SdtHeader::new(pointer) } {
                match unsafe { str::from_utf8_unchecked(&header.signature) } {
                    Hpet::SIGNATURE => acpi.hpet = Hpet::new(header),
                    s => warn!("\t\t* {} (unhandled)", s),
                }
            } else {
                warn!("Could not map the header for an ACPI table at {}.", pointer);
            }
        }
    }
}
