pub mod msr {
    pub const FS_BASE: u32 = 0xC000_0100;
    pub const TSC_DEADLINE: u32 = 0x6E0;
    pub const APIC_BASE: u32 = 0x1B;
}

macro_rules! reg_access {
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
            pub fn $name(value: $t) {
                unsafe { llvm_asm!(concat!("mov ", stringify!($name), ", $0") :: "r"(value) :: "intel", "volatile") };
            }
    };
}

pub mod read {
    reg_access!(cr2, usize, r);

    #[inline(always)]
    pub fn msr(address: u32) -> u64 {
        let ret_low: u32;
        let ret_high: u32;
        unsafe { asm!("rdmsr", in("ecx") address, out("edx") ret_high, out("eax") ret_low) };
        ((ret_high as u64) << 32) | ret_low as u64
    }

    pub fn apic_base() -> u64 {
        msr(super::msr::APIC_BASE)
    }

    pub fn tsc_deadline() -> u64 {
        msr(super::msr::TSC_DEADLINE)
    }
}

pub mod write {
    use crate::memory::VirtualAddress;

    /// # Safety
    /// Caller must guarantee safety in the context of the particular MSR being written.
    #[inline(always)]
    pub unsafe fn msr(address: u32, v: u64) {
        let v_low = v as u32;
        let v_high = v >> 32 as u32;

        asm!("wrmsr", in("ecx") address, in("edx") v_high, in("eax") v_low);
    }

    /// # Safety
    /// Caller must guarantee that `address` is a valid base for relative addressing.
    #[inline(always)]
    pub unsafe fn fs_base(address: VirtualAddress) {
        msr(super::msr::FS_BASE, *address as u64)
    }

    pub unsafe fn tsc_deadline(value: u64) {
        msr(super::msr::TSC_DEADLINE, value);
    }
}

/*pub fn msr_read(addr: u32) -> u64 {
    let ret_low: u32;
    let ret_high: u32;
    unsafe {
        llvm_asm!("rdmsr"
             :
             "={edx}"(ret_high)
             "={eax}"(ret_low)
             :
             "{ecx}"(addr)
             ::
             "intel")
    };
    let mut ret = ret_low as u64;
    ret |= (ret_high as u64) << 32;
    ret
}

pub fn star_read() -> u64 {
    msr_read(0xC000_0081)
}
*/
