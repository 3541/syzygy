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

pub struct Mmap<'a>(pub &'a [MmapEntry]);
