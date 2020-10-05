use crate::mem::VirtualAddress;

#[repr(packed)]
struct Gdtr {
    size: u16,
    address: VirtualAddress,
}

#[repr(packed)]
struct SegmentDescriptor {
    _limit_low: u16,
    _base_low: u16,
    _base_mid: u8,
    access: u8,
    flags: u8,
    _base_high: u8,
}

//struct Gdt([SegmentDescriptor
