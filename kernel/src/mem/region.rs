use super::VirtualAddress;

#[derive(Copy, Clone)]
pub struct VirtualRegion {
    start: VirtualAddress,
    size: usize,
}

impl VirtualRegion {
    pub const fn new(start: VirtualAddress, size: usize) -> VirtualRegion {
        VirtualRegion { start, size }
    }
}
