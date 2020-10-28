use core::mem::transmute;
use core::{slice, str};

use crate::mem::map::{Mmap, MmapEntry, MmapEntryType};
use crate::mem::{Address, PhysicalAddress};

// Despite the #[repr(packed)], these structures are actually safe to access,
// since they are set up to be correctly-aligned regardless (thanks ).

#[allow(unused)]
#[derive(PartialEq)]
#[repr(u64)]
pub enum TagIdentifier {
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

pub trait TagInner {
    const IDENTIFIER: TagIdentifier;
}

struct NullTag {}

impl TagInner for NullTag {
    const IDENTIFIER: TagIdentifier = TagIdentifier::Invalid;
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
    fn parse(&self) -> MmapEntryType {
        match self {
            Self::Usable => MmapEntryType::Usable,
            Self::Reserved
            | Self::AcpiReclaimable
            | Self::AcpiNvs
            | Self::Bad
            | Self::BootloaderReclaimable => MmapEntryType::Reserved,
            Self::KernelAndModules => MmapEntryType::Kernel,
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

    fn mmap(&self) -> MmapIterator<'static> {
        MmapIterator::new(self.items())
    }
}

pub struct MmapIterator<'a> {
    entries: &'a [MmapTagEntry],
    current: usize,
}

impl MmapIterator<'_> {
    fn new(entries: &[MmapTagEntry]) -> MmapIterator {
        MmapIterator {
            entries,
            current: 0,
        }
    }
}

impl Iterator for MmapIterator<'_> {
    type Item = MmapEntry;

    fn next(&mut self) -> Option<MmapEntry> {
        if self.current >= self.entries.len() {
            None
        } else {
            let ret = self.entries[self.current].parse();
            self.current += 1;
            Some(ret)
        }
    }
}

impl TagInner for MmapTag {
    const IDENTIFIER: TagIdentifier = TagIdentifier::Mmap;
}

#[repr(packed)]
pub struct Tag<T: TagInner> {
    identifier: TagIdentifier,
    next: Option<&'static Tag<NullTag>>,
    inner: T,
}

// The top-level structure returned by the bootloader.
#[repr(packed)]
pub struct StivaleInfo {
    brand: [u8; 64],
    version: [u8; 64],
    tags: &'static Tag<NullTag>,
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

    fn get_tag<T: TagInner>(&self) -> Option<&Tag<T>> {
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

    pub fn mmap(&self) -> Mmap {
        self.get_tag::<MmapTag>()
            .expect("Couldn't find mmap tag.")
            .inner
            .mmap()
            .collect()
    }
}
