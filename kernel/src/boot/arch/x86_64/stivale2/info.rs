//! Parsing of Stivale2 [info tags](https://github.com/stivale/stivale/blob/master/STIVALE2.md#stivale2-structure).

use core::mem::transmute;
use core::{ptr, slice, str};

use crate::mem::map::{Mmap, MmapEntry, MmapEntryType};
use crate::mem::{Address, PhysicalAddress};

// Despite the #[repr(packed)], these structures are actually safe to access,
// since they are set up to be correctly-aligned regardless.

/// Structure tag identifiers.
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
    Slide = 0xEE80847D01506C57,
    Uart = 0xB813F9B8DBC78797,
    Dtb = 0xABB29BD49A2833FA,
}

/// Associates a tag type with its identifier.
pub trait TagInner {
    const IDENTIFIER: TagIdentifier;
}

/// Used for a tag with as yet unknown type.
struct NullTag {}

impl TagInner for NullTag {
    const IDENTIFIER: TagIdentifier = TagIdentifier::Invalid;
}

/// Memory area types.
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

/// A memory area in the [MmapTag](MmapTag).
#[repr(C, packed)]
struct MmapTagEntry {
    base: u64,
    length: u64,
    entry_type: MmapTagEntryType,
    _unused: u32,
}

impl MmapTagEntry {
    fn entry_type(&self) -> MmapTagEntryType {
        // SAFETY: entry_type is valid and initialized.
        unsafe { ptr::read_unaligned(ptr::addr_of!(self.entry_type)) }
    }

    fn parse(&self) -> MmapEntry {
        MmapEntry {
            entry_type: self.entry_type().parse(),
            start: PhysicalAddress::new(self.base as usize),
            size: self.length as usize,
        }
    }
}

/// The memory map.
#[repr(C, packed)]
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

    /// Get the memory map as an iterator.
    fn mmap(&self) -> MmapIterator<'static> {
        MmapIterator::new(self.items())
    }
}

/// An iterator over the entries of the memory map. Each entry is parsed into an [MmapEntry](MmapEntry) before being returned.
pub struct MmapIterator<'a> {
    entries: &'a [MmapTagEntry],
    index: usize,
}

impl MmapIterator<'_> {
    fn new(entries: &[MmapTagEntry]) -> MmapIterator {
        MmapIterator { entries, index: 0 }
    }
}

impl Iterator for MmapIterator<'_> {
    type Item = MmapEntry;

    fn next(&mut self) -> Option<MmapEntry> {
        if self.index >= self.entries.len() {
            None
        } else {
            let ret = self.entries[self.index].parse();
            self.index += 1;
            Some(ret)
        }
    }
}

impl TagInner for MmapTag {
    const IDENTIFIER: TagIdentifier = TagIdentifier::Mmap;
}

#[repr(C, packed)]
struct SlideTag {
    slide: u64,
}

impl TagInner for SlideTag {
    const IDENTIFIER: TagIdentifier = TagIdentifier::Slide;
}

/// A (potentially typed) info tag.
#[repr(C, packed)]
pub struct Tag<T: TagInner> {
    identifier: TagIdentifier,
    next: Option<&'static Tag<NullTag>>,
    inner: T,
}

impl<T: TagInner> Tag<T> {
    fn identifier(&self) -> TagIdentifier {
        // SAFETY: identifier is valid and initialized.
        unsafe { ptr::read_unaligned(ptr::addr_of!(self.identifier)) }
    }
}

/// The top-level structure returned by the bootloader.
#[repr(C, packed)]
pub struct StivaleInfo {
    /// The name of the bootloader.
    brand: [u8; 64],
    /// The version of the bootloader.
    version: [u8; 64],
    /// A linked list of info tags.
    tags: &'static Tag<NullTag>,
}

/// Utility function to turn a null-terminated byte string into a correctly-sized slice.
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

    /// Get the first tag of type `T`.
    fn get_tag<T: TagInner>(&self) -> Option<&Tag<T>> {
        let mut current = self.tags;

        loop {
            if current.identifier() == T::IDENTIFIER {
                return unsafe { Some(transmute(current)) };
            }

            if let Some(tag) = current.next {
                current = tag;
            } else {
                break;
            }
        }

        None
    }

    /// Get the memory map.
    pub fn mmap(&self) -> Mmap {
        self.get_tag::<MmapTag>()
            .expect("Couldn't find mmap tag.")
            .inner
            .mmap()
            .collect()
    }

    /// Get the kernel slide.
    pub fn slide(&self) -> u64 {
        self.get_tag::<SlideTag>()
            .expect("Couldn't find kernel slide tag.")
            .inner
            .slide
    }
}
