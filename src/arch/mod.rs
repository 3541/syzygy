#[cfg(target_arch = "x86_64")]
pub mod x86_64;
#[cfg(target_arch = "x86_64")]
pub use x86_64 as native;

#[cfg(target_arch = "x86")]
pub mod i686;
#[cfg(target_arch = "x86")]
pub use x86 as native;

pub use native::constants;