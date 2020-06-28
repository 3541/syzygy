/*macro_rules! gpr_access {
    ($name:ident, $t:ty, read) => {
        pub mod read {
            pub fn $name() -> $t {
                let ret;
                unsafe { llvm_asm!(concat!("mov $0, ", stringify!($name)) : "=r"(ret) ::: "intel") };
                ret
            }
        }
    }
}*/

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
