#[cfg(target_arch = "x86_64")]
mod x86_64;
#[cfg(target_arch = "x86_64")]
pub use x86_64::X86_64CpuState as CpuSaveState;
#[cfg(target_arch = "x86_64")]
pub use x86_64::*;
