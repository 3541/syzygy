pub struct MemoryRegion {
    start: Page,
    size: usize,
    flags: EntryFlags,
}

impl MemoryRegion {
    pub fn new(start_address: VirtualAddress, size: usize, flags: EntryFlags) -> Self {}
}
