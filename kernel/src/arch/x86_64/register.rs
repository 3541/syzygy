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

    pub fn msr(address: u32) -> u64 {
        let ret_low: u32;
        let ret_high: u32;
        unsafe { asm!("rdmsr", in("ecx") address, out("edx") ret_high, out("eax") ret_low) };
        ((ret_high as u64) << 32) | ret_low as u64
    }

    pub fn apic_base() -> u64 {
        msr(0x1B)
    }
}

pub mod write {
    use crate::memory::VirtualAddress;

    /// # Safety
    /// Caller must guarantee safety in the context of the particular MSR being written.
    pub unsafe fn msr(address: u32, v: u64) {
        let v_low = v as u32;
        let v_high = v >> 32 as u32;

        asm!("wrmsr", in("ecx") address, in("edx") v_high, in("eax") v_low);
    }

    /// # Safety
    /// Caller must guarantee that `address` is a valid base for relative addressing.
    pub unsafe fn fs_base(address: VirtualAddress) {
        msr(0xC000_0100, *address as u64)
    }

    pub unsafe fn tsc_deadline(value: u64) {
        msr(0x6E0, value);
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
