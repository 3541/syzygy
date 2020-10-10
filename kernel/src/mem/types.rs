use core::fmt::{self, Display, Formatter, Debug};
use core::ops::{Add, Sub};
use core::mem::transmute;

pub mod size {
    use core::mem::size_of;

    pub const KB: usize = 1024;

    // The number of T-sized things which can fit in a given number of bytes.
    pub const fn units_of<T>(bytes: usize) -> usize {
        bytes / size_of::<T>()
    }
}

pub type RawPhysicalAddress = usize;
pub type RawVirtualAddress = usize;

#[inline]
pub const fn align_down(n: usize, align: usize) -> usize {
    n & !(align - 1)
}

#[inline]
pub const fn align_up(n: usize, align: usize) -> usize {
    align_down(n + align - 1, align)
}

#[inline]
const fn address_is_valid(address: usize) -> bool {
    address < VirtualAddress::NONCANONICAL_START || address >= VirtualAddress::NONCANONICAL_END
}

pub trait Address: Clone + Display {
    type RawAddress: PartialOrd<usize> + Into<usize> + From<usize>;

    const NONCANONICAL_START: usize = 0x0000_8000_0000_0000;
    const NONCANONICAL_END: usize = 0xFFFF_8000_0000_0000;

    fn raw(&self) -> Self::RawAddress;
    unsafe fn new_unchecked(address: Self::RawAddress) -> Self;

    #[inline]
    fn is_valid(&self) -> bool {
        address_is_valid(self.raw().into())
    }

    fn is_aligned(&self, align: usize) -> bool {
        self.raw().into() % align == 0
    }

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

#[derive(Debug, Copy, Clone)]
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

impl Display for PhysicalAddress {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "PhysicalAddress(0x{:x})", self.raw())
    }
}

#[derive(Copy, Clone, PartialOrd, PartialEq)]
pub struct VirtualAddress(RawVirtualAddress);

impl VirtualAddress {
    pub const unsafe fn from_ptr_unchecked<T>(ptr: *const T) -> VirtualAddress {
        let ret = VirtualAddress::new_unchecked(transmute(ptr));
        ret
    }

    pub fn from_ptr<T>(ptr: *const T) -> VirtualAddress {
        let ret = unsafe { VirtualAddress::from_ptr_unchecked(ptr) };
        assert!(ret.is_valid());
        ret
    }

    pub fn as_mut_ptr<T>(&mut self) -> *mut T {
        self.raw() as *mut _
    }

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
            (_, true) => panic!("Virtual address subtraction overflowed.")
        }
    }
}
