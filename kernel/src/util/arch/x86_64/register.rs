macro_rules! impl_reg_access {
    ($name:ident, $t:ty, r) => {
            #[inline(always)]
            pub fn $name() -> $t {
                let ret;
                unsafe { llvm_asm!(concat!("mov $0, ", stringify!($name)) : "=r"(ret) ::: "intel") };
                ret
            }
    };

    ($name:ident, $t:ty, w) => {
            #[inline(always)]
            pub unsafe fn $name(value: $t) {
                llvm_asm!(concat!("mov ", stringify!($name), ", $0") :: "r"(value) :: "intel", "volatile");
            }
    };
}

pub mod msr {
    use bitflags::bitflags;

    pub const EFER: u32 = 0xC0000080;

    bitflags! {
        pub struct EferFlags: u64 {
            const NXE = 1 << 11;
        }
    }
}

pub mod read {
    use super::msr;

    impl_reg_access!(cs, u16, r);

    pub fn msr(address: u32) -> u64 {
        let ret_low: u32;
        let ret_high: u32;
        unsafe { asm!("rdmsr", in("ecx") address, out("edx") ret_high, out("eax") ret_low) };
        ((ret_high as u64) << 32) | ret_low as u64
    }

    pub fn efer() -> msr::EferFlags {
        // Unchecked so that flags which are not represented here are preserved.
        unsafe { msr::EferFlags::from_bits_unchecked(msr(msr::EFER)) }
    }
}

pub mod write {
    use super::msr;

    pub unsafe fn msr(address: u32, val: u64) {
        let val_low = val as u32;
        let val_high = val >> 32 as u32;
        asm!("wrmsr", in("ecx") address, in("edx") val_high, in("eax") val_low);
    }

    pub unsafe fn efer(value: msr::EferFlags) {
        msr(msr::EFER, value.bits())
    }
}
