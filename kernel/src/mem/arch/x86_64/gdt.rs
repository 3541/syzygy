use core::mem::size_of;

use bitflags::bitflags;

use crate::mem::VirtualAddress;
use crate::util::PrivilegeLevel;

// Most of the segment descriptor is ignored in long mode. All that needs to be
// set are the access permissions and segment types.
#[allow(unused)]
#[repr(packed)]
struct SegmentDescriptor {
    _limit_low: u16,
    _base_low: u16,
    _base_mid: u8,
    flags: u16,
    _base_high: u8,
}

bitflags! {
    struct SegmentFlags: u16 {
        const WRITABLE = 1 << 1; // Only applicable to data segments.
        const EXECUTABLE = 1 << 3;
        const MEMORY = 1 << 4; // Not a system segment.
        const PRESENT = 1 << 7;
        const LONG_CODE = 1 << 13; // 64-bit code segment.
    }
}

impl SegmentDescriptor {
    const fn new(privilege: PrivilegeLevel, flags: SegmentFlags) -> SegmentDescriptor {
        SegmentDescriptor {
            _limit_low: 0,
            _base_low: 0,
            _base_mid: 0,
            flags: flags.bits() | ((privilege as u16) << 5),
            _base_high: 0,
        }
    }

    const fn null() -> SegmentDescriptor {
        SegmentDescriptor::new(PrivilegeLevel::Kernel, SegmentFlags::empty())
    }
}

struct Gdt<const DESCRIPTORS: usize>([SegmentDescriptor; DESCRIPTORS]);

#[allow(unused)]
#[repr(packed)]
struct Gdtr {
    size: u16,
    address: VirtualAddress,
}

impl<const DESCRIPTORS: usize> Gdt<DESCRIPTORS> {
    unsafe fn load(&self) {
        let r = Gdtr {
            size: (size_of::<Self>() - 1) as u16,
            address: VirtualAddress::from_ptr(&self.0[0]),
        };

        // This depends on KernelCode being the second element of the GDT.
        asm!(
            "lgdt [{0}]",
            "mov ss, {1:x}",
            "mov ds, {1:x}",
            "mov es, {1:x}",
            in(reg) &r,
            in(reg) GdtIndex::KernelData.selector()
        );
    }
}

// Note: This enum is for convenience and is specific to the particular GDT
// instantiated below.
#[allow(unused)]
#[repr(u8)]
#[derive(Copy, Clone)]
enum GdtIndex {
    Null = 0,
    KernelCode = 1,
    KernelData = 2,
}

impl GdtIndex {
    // The value to load into a selector register for this index.
    fn selector(&self) -> u16 {
        *self as u16 * size_of::<usize>() as u16
    }
}

// This is ugly because BitOr isn't currently const. It seems this is waiting on
// https://github.com/bitflags/bitflags/pull/217 to merge.
static GDT: Gdt<3> = Gdt([
    SegmentDescriptor::null(),
    // Kernel code.
    SegmentDescriptor::new(
        PrivilegeLevel::Kernel,
        SegmentFlags::from_bits_truncate(
            SegmentFlags::PRESENT.bits()
                | SegmentFlags::MEMORY.bits()
                | SegmentFlags::EXECUTABLE.bits()
                | SegmentFlags::LONG_CODE.bits(),
        ),
    ),
    // Kernel data.
    SegmentDescriptor::new(
        PrivilegeLevel::Kernel,
        SegmentFlags::from_bits_truncate(
            SegmentFlags::PRESENT.bits()
                | SegmentFlags::MEMORY.bits()
                | SegmentFlags::WRITABLE.bits(),
        ),
    ),
]);

pub fn init() {
    unsafe { GDT.load() };
}
