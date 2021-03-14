//! Basic types and utilities for dealing with memory.

use core::fmt::{self, Debug, Display, Formatter};
use core::ops::{Add, Sub};

/// Convenience constants and functions for dealing with memory sizes.
pub mod size {
    use core::mem::size_of;

    /// The number of bytes in 1 KiB.
    pub const KB: usize = 1024;

    /// The number of T-sized things which can fit in a given number of bytes.
    pub const fn units_of<T>(bytes: usize) -> usize {
        bytes / size_of::<T>()
    }
}

/// A raw physical address.
pub type RawPhysicalAddress = usize;
/// A raw virtual address.
pub type RawVirtualAddress = usize;

/// The previous number with the given alignment.
#[inline]
pub const fn align_down(n: usize, align: usize) -> usize {
    n & !(align - 1)
}

/// The next number with the given alignment.
#[inline]
pub const fn align_up(n: usize, align: usize) -> usize {
    align_down(n + align - 1, align)
}

/// Check whether an address is a canonical valid address.
#[inline]
const fn address_is_valid(address: usize) -> bool {
    address < VirtualAddress::NONCANONICAL_START || address >= VirtualAddress::NONCANONICAL_END
}

/// A physical or virtual address.
pub trait Address: Clone + Display {
    /// The underlying numeric type.
    type RawAddress: PartialOrd<usize> + Into<usize> + From<usize>;

    /// The start of the noncanonical hole in the address space.
    const NONCANONICAL_START: usize = 0x0000_8000_0000_0000;
    /// The end of the noncanonical hole in the address space.
    const NONCANONICAL_END: usize = 0xFFFF_8000_0000_0000;

    /// The underlying address.
    fn raw(&self) -> Self::RawAddress;
    /// Create an address without validation.
    unsafe fn new_unchecked(address: Self::RawAddress) -> Self;

    /// Check whether the address is valid.
    #[inline]
    fn is_valid(&self) -> bool {
        address_is_valid(self.raw().into())
    }

    /// Check whether the address has the given alignment.
    fn is_aligned(&self, align: usize) -> bool {
        self.raw().into() % align == 0
    }

    /// Create a new address. Enforces canonicity.
    fn new(address: Self::RawAddress) -> Self {
        let ret = unsafe { Self::new_unchecked(address) };
        assert!(
            ret.is_valid(),
            "Invalid address {}. Address is in noncanonical range 0x{:x}-0x{:x}.",
            ret,
            Self::NONCANONICAL_START,
            Self::NONCANONICAL_END
        );

        ret
    }
}

/// A physical address.
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub struct PhysicalAddress(RawPhysicalAddress);

impl const Address for PhysicalAddress {
    type RawAddress = RawPhysicalAddress;

    unsafe fn new_unchecked(address: RawPhysicalAddress) -> PhysicalAddress {
        PhysicalAddress(address)
    }

    fn raw(&self) -> RawPhysicalAddress {
        self.0
    }
}

impl Add<usize> for PhysicalAddress {
    type Output = PhysicalAddress;

    fn add(self, rhs: usize) -> PhysicalAddress {
        PhysicalAddress::new(
            self.0
                .checked_add(rhs)
                .expect("Physical address addition overflowed."),
        )
    }
}

impl const Sub<PhysicalAddress> for PhysicalAddress {
    type Output = RawPhysicalAddress;

    fn sub(self, rhs: PhysicalAddress) -> RawPhysicalAddress {
        match self.0.overflowing_sub(rhs.0) {
            (v, false) => v,
            (_, true) => panic!("Physical address subtraction overflowed."),
        }
    }
}

impl Display for PhysicalAddress {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "PhysicalAddress(0x{:x})", self.raw())
    }
}

/// A virtual address.
#[derive(Copy, Clone, PartialOrd, PartialEq)]
pub struct VirtualAddress(RawVirtualAddress);

impl VirtualAddress {
    /// Create a virtual address from a pointer with no validation.
    /// # Safety
    /// `ptr` must be a valid pointer to an extant memory location (and
    /// therefore a canonical address).
    pub const unsafe fn from_ptr_unchecked<T>(ptr: *const T) -> VirtualAddress {
        VirtualAddress::new_unchecked(ptr as usize)
    }

    /// Create a virtual address from a pointer.
    pub fn from_ptr<T>(ptr: *const T) -> VirtualAddress {
        let ret = unsafe { VirtualAddress::from_ptr_unchecked(ptr) };
        assert!(ret.is_valid());
        ret
    }

    /// Create a mutable pointer to `T` from the address.
    pub fn as_mut_ptr<T>(&self) -> *mut T {
        self.raw() as *mut _
    }

    /// Get the next address of the given alignment.
    pub const fn next_aligned(&self, align: usize) -> Self {
        let ret = unsafe {
            Self::new_unchecked(align_up(self.raw(), align))
        };
        assert!(address_is_valid(ret.raw()));

        ret
    }
}

impl const Address for VirtualAddress {
    type RawAddress = RawVirtualAddress;

    unsafe fn new_unchecked(address: RawVirtualAddress) -> VirtualAddress {
        VirtualAddress(address)
    }

    fn raw(&self) -> RawVirtualAddress {
        self.0
    }
}

impl Display for VirtualAddress {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "VirtualAddress(0x{:x})", self.raw())
    }
}

impl Debug for VirtualAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <VirtualAddress as Display>::fmt(self, f)
    }
}

impl Add<usize> for VirtualAddress {
    type Output = VirtualAddress;

    fn add(self, rhs: usize) -> VirtualAddress {
        VirtualAddress::new(
            self.0
                .checked_add(rhs)
                .expect("Virtual address addition overflowed."),
        )
    }
}

impl const Sub<VirtualAddress> for VirtualAddress {
    type Output = RawVirtualAddress;

    fn sub(self, rhs: VirtualAddress) -> RawVirtualAddress {
        match self.0.overflowing_sub(rhs.0) {
            (v, false) => v,
            (_, true) => panic!("Virtual address subtraction overflowed."),
        }
    }
}
