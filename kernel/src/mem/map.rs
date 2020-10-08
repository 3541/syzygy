use super::PhysicalAddress;

// Memory map used for initializing memory management.
#[derive(Debug, Copy, Clone)]
pub enum MmapType {
    Usable,
    Reserved,
    Kernel,
}

#[derive(Debug, Copy, Clone)]
pub struct MmapEntry {
    pub entry_type: MmapType,
    pub start: PhysicalAddress,
    pub size: usize,
}

pub struct Mmap<T: Iterator<Item = MmapEntry>>(pub T);

impl<T: Iterator<Item = MmapEntry>> Iterator for Mmap<T> {
    type Item = MmapEntry;

    fn next(&mut self) -> Option<MmapEntry> {
        self.0.next()
    }
}
