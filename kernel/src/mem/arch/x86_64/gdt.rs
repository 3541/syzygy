//! The global descriptor table.

use core::mem::size_of;

use bitflags::bitflags;

use crate::mem::VirtualAddress;
use crate::util::PrivilegeLevel;

/// A segment descriptor. Most of this is ignored in long mode.
#[allow(unused)]
#[repr(packed)]
struct SegmentDescriptor {
    _limit_low: u16,
    _base_low: u16,
    _base_mid: u8,
    /// Access permissions and segment type.
    flags: u16,
    _base_high: u8,
}

bitflags! {
    /// Segment types and permissions.
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

/// The global descriptor table.
struct Gdt<const DESCRIPTORS: usize>([SegmentDescriptor; DESCRIPTORS]);

/// The GDT register.
#[allow(unused)]
#[repr(packed)]
struct Gdtr {
    /// The length of the GDT.
    size: u16,
    /// A pointer to the GDT.
    address: VirtualAddress,
}

impl<const DESCRIPTORS: usize> Gdt<DESCRIPTORS> {
    /// Load the GDT and reload segment selectors.
    /// # Safety
    /// The loaded GDT must be valid, complete, and have the kernel code segment as the second element.
    unsafe fn load(&self) {
        let r = Gdtr {
            size: (size_of::<Self>() - 1) as u16,
            address: VirtualAddress::from_ptr(&self.0[0]),
        };

        asm!(
            "lgdt [{0}]",
            "mov ss, {1:x}",
            "mov ds, {1:x}",
            "mov es, {1:x}",
            in(reg) &r,
            in(reg) GdtIndex::KernelData.selector()
        );

        // This depends on KernelCode being the second element of the GDT.
        asm!(
            "push {0:r}",
            "lea {1}, [rip + 1f]",
            "push {1}",
            "retfq",
            "1:",
            in(reg) GdtIndex::KernelCode.selector(),
            lateout(reg) _
        );
    }
}

/// Index into the default GDT layout.
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
/// The one and only GDT.
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

/// Load the new GDT.
pub fn init() {
    unsafe { GDT.load() };
}
