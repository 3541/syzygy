use core::mem::transmute;
use core::{slice, str};

use crate::mem::map::{Mmap, MmapEntry, MmapType};
use crate::mem::{Address, PhysicalAddress};

// Despite the #[repr(packed)], these structures are actually safe to access,
// since they are set up to be correctly-aligned regardless (thanks Stivale).

#[allow(unused)]
#[derive(PartialEq)]
#[repr(u64)]
pub enum StivaleTagIdentifier {
    Invalid = 0,
    CommandLine = 0xE5E76A1B4597A781,
    Mmap = 0x2187F79E8612DE07,
    Epoch = 0x566A7BED888E1407,
    Rsdp = 0x9E1786930A375E78,
    Modules = 0x4B6FE466AADE04CE,
    Firmware = 0x359D837855E3858C,
    Framebuffer = 0x506461D2950408FA,
    Smp = 0x34D1D96339647025,
    Uart = 0xB813F9B8DBC78797,
    Dtb = 0xABB29BD49A2833FA,
}

pub trait StivaleTagInner {
    const IDENTIFIER: StivaleTagIdentifier;
}

struct NullTag {}

impl StivaleTagInner for NullTag {
    const IDENTIFIER: StivaleTagIdentifier = StivaleTagIdentifier::Invalid;
}

#[allow(unused)]
#[repr(u32)]
enum MmapTagEntryType {
    Usable = 1,
    Reserved = 2,
    AcpiReclaimable = 3,
    AcpiNvs = 4,
    Bad = 5,
    BootloaderReclaimable = 0x1000,
    KernelAndModules = 0x1001,
}

impl MmapTagEntryType {
    fn parse(&self) -> MmapType {
        match self {
            Self::Usable => MmapType::Usable,
            Self::Reserved
            | Self::AcpiReclaimable
            | Self::AcpiNvs
            | Self::Bad
            | Self::BootloaderReclaimable => MmapType::Reserved,
            Self::KernelAndModules => MmapType::Kernel,
        }
    }
}

#[repr(packed)]
struct MmapTagEntry {
    base: u64,
    length: u64,
    entry_type: MmapTagEntryType,
    _unused: u32,
}

impl MmapTagEntry {
    fn parse(&self) -> MmapEntry {
        MmapEntry {
            entry_type: unsafe { self.entry_type.parse() },
            start: PhysicalAddress::new(self.base as usize),
            size: self.length as usize,
        }
    }
}

#[repr(packed)]
struct MmapTag {
    entries: u64,
    // map: [MmapTagEntry; entries]
}

impl MmapTag {
    // To avoid DST annoyances.
    fn items(&self) -> &'static [MmapTagEntry] {
        unsafe {
            slice::from_raw_parts(
                (self as *const MmapTag).offset(1) as *const MmapTagEntry,
                self.entries as usize,
            )
        }
    }

    fn mmap(&self) -> StivaleMmap<'static> {
        StivaleMmap::new(self.items())
    }
}

pub struct StivaleMmap<'a> {
    entries: &'a [MmapTagEntry],
    current: usize,
}

impl StivaleMmap<'_> {
    fn new(entries: &[MmapTagEntry]) -> StivaleMmap {
        StivaleMmap {
            entries,
            current: 0,
        }
    }
}

impl Iterator for StivaleMmap<'_> {
    type Item = MmapEntry;

    fn next(&mut self) -> Option<MmapEntry> {
        let ret = self.entries[self.current].parse();
        self.current += 1;
        Some(ret)
    }
}

impl StivaleTagInner for MmapTag {
    const IDENTIFIER: StivaleTagIdentifier = StivaleTagIdentifier::Mmap;
}

#[repr(packed)]
pub struct StivaleTag<T: StivaleTagInner> {
    identifier: StivaleTagIdentifier,
    next: Option<&'static StivaleTag<NullTag>>,
    inner: T,
}

// The top-level structure returned by the bootloader.
#[repr(packed)]
pub struct StivaleInfo {
    brand: [u8; 64],
    version: [u8; 64],
    tags: &'static StivaleTag<NullTag>,
}

fn from_null_terminated(str: &[u8]) -> &[u8] {
    let mut len = 0;
    for b in str {
        if *b == 0 {
            break;
        }
        len += 1;
    }
    &str[..len]
}

impl StivaleInfo {
    pub fn brand(&self) -> &str {
        unsafe { str::from_utf8_unchecked(from_null_terminated(&self.brand)) }
    }

    pub fn version(&self) -> &str {
        unsafe { str::from_utf8_unchecked(from_null_terminated(&self.version)) }
    }

    fn get_tag<T: StivaleTagInner>(&self) -> Option<&StivaleTag<T>> {
        let mut current = self.tags;

        loop {
            if unsafe { current.identifier == T::IDENTIFIER } {
                return Some(unsafe { transmute(current) });
            }

            if let Some(tag) = current.next {
                current = tag;
            } else {
                break;
            }
        }

        None
    }

    pub fn mmap(&self) -> Mmap<StivaleMmap> {
        Mmap(
            self.get_tag::<MmapTag>()
                .expect("Couldn't find mmap tag.")
                .inner
                .mmap(),
        )
    }
}
