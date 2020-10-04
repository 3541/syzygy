use crate::mem::VirtualAddress;

#[allow(unused)]
#[repr(packed)]
pub struct StivaleHeader {
    // Entry point is to be determined by ELF.
    _entry_point: u64,
    stack: *const u8,
    flags: u64,
    tags: VirtualAddress,
}

// Required to be used in static context. Invariants preserved since mutation is
// forbidden.
unsafe impl Send for StivaleHeader {}
unsafe impl Sync for StivaleHeader {}

impl StivaleHeader {
    pub const unsafe fn new(stack: *const u8, kaslr: bool) -> StivaleHeader {
        StivaleHeader {
            _entry_point: 0,
            stack,
            flags: if kaslr { 1 } else { 0 },
            tags: VirtualAddress::new_const(0),
        }
    }
}
