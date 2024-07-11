macro_rules! arch_mod_impl {
    ($arch:ident, $arch_str:expr) => {
        #[cfg(target_arch = $arch_str)]
        mod $arch;
        #[cfg(target_arch = $arch_str)]
        pub use $arch::*;
    };
}

macro_rules! arch_mod {
    () => {
        arch_mod_impl!(amd64, "x86_64");
        arch_mod_impl!(aarch64, "aarch64");
    };
}
