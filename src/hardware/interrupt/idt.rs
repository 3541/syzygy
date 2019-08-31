struct IDT([IDTEntry; 16]);

#[repr(C, packed)]
struct IDTPointer {
    size: u16,
    #[cfg(target_arch = "x86_64")]
    offset: u64,
    #[cfg(target_arch = "x86")]
    offset: u32,
}

#[repr(C, packed)]
#[cfg(target_arch = "x86_64")]
struct IDTEntry {
    offset_low: u16,
    selector: u16,
    ist_offset: u8,
    options: u8,
    offset_mid: u16,
    offset_high: u32,
    _zero: u32,
}

#[repr(C, packed)]
#[cfg(target_arch = "x86")]
struct IDTEntry {
    offset_low: u16,
    selector: u16,
    _zero: u8,
    options: u8,
    offset_high: u16,
}
