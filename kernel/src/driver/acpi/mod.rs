use logc::{debug, error, warn};
use multiboot2::{RsdpV1Tag, RsdpV2Tag};

use crate::memory::{Address, PhysicalAddress};

pub fn init(rsdp_v1: Option<&RsdpV1Tag>, rsdp_v2: Option<&RsdpV2Tag>) {
    if let Some(_rsdp_v2) = rsdp_v2 {
        debug!("Got RSDPv2");
        warn!("This is currently not handled.");
    } else if let Some(rsdp_v1) = rsdp_v1 {
        debug!("Got RSDPv1");

        let signature = rsdp_v1.signature().unwrap();
        let oem_id = rsdp_v1.oem_id().unwrap();
        let revision = rsdp_v1.revision();
        let rsdt_address = PhysicalAddress::new(rsdp_v1.rsdt_address());
        debug!("\tSignature: {}", signature);
        debug!("\tOEM ID: {}", oem_id);
        debug!("\tRevision: {}", revision);
        debug!("\tRSDT address: {}", rsdt_address);

        let checksum = rsdp_v1.checksum();
        debug!("\tChecksum: {}", checksum);

        let sum: u8 = signature
            .bytes()
            .chain(oem_id.bytes())
            .chain(rsdt_address.to_le_bytes().iter().map(|b| *b))
            .fold(0, |s: u8, b| s.overflowing_add(b).0)
            .overflowing_add(revision)
            .0
            .overflowing_add(checksum)
            .0;
        if sum != 0 {
            error!("Checksum invalid: {}", sum);
            return;
        } else {
            debug!("\t\tValid.");
        }
    } else {
        debug!("No RSDP");
        warn!("This is currently not handled.");
    }
}
