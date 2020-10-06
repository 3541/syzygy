use core::fmt::{self, Display, Formatter};
use core::ops::Add;

pub type RawPhysicalAddress = usize;
pub type RawVirtualAddress = usize;

pub trait Address: Clone + Display {
    type RawAddress: PartialOrd<usize>;

    const SIGN_EX_INVALID_START: usize = 0x0000_8000_0000_0000;
    const SIGN_EX_INVALID_END: usize = 0xFFFF_8000_0000_0000;

    fn raw(&self) -> Self::RawAddress;
    unsafe fn new_unchecked(address: Self::RawAddress) -> Self;

    #[inline]
    fn is_valid(&self) -> bool {
        self.raw() < Self::SIGN_EX_INVALID_START || self.raw() >= Self::SIGN_EX_INVALID_END
    }

    fn new(address: Self::RawAddress) -> Self {
        let ret = unsafe { Self::new_unchecked(address) };
        assert!(
            ret.is_valid(),
            "Invalid address {}. Sign extension not set correctly.",
            ret
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

#[derive(Debug, Copy, Clone)]
pub struct VirtualAddress(RawVirtualAddress);

impl VirtualAddress {
    pub fn from_ptr<T>(ptr: *const T) -> VirtualAddress {
        VirtualAddress::new(ptr as usize)
    }

    pub fn as_mut_ptr<T>(&mut self) -> *mut T {
        self.raw() as *mut _
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
