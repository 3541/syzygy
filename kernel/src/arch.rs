//! See [arch_submodules](arch_submodules).

/// Utility macro for generating and using architecture-specific submodules.
#[macro_export]
macro_rules! arch_submodules {
    ($arch:ident, $arch_str:expr) => {
        cfg_if::cfg_if! {
            if #[cfg(target_arch = $arch_str)] {
                mod $arch;
                pub use $arch::*;
            }
        }
    };

    () => {
        arch_submodules!(x86_64, "x86_64");
    };
}
