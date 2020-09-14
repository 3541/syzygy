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
            pub fn $name(value: $t) {
                unsafe { llvm_asm!(concat!("mov ", stringify!($name), ", $0") :: "r"(value) :: "intel", "volatile") };
            }
    };
}

pub mod read {
    impl_reg_access!(cs, u16, r);
}
