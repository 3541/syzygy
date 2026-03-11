use crate::mmap::Mmap;

pub const ENTRYPOINT: &'static str = "kinit";

// This extern is only required to match ABI between the Rust code in the EFI loader and the kernel,
// not for actual FFI. Both sides agree on what Mmap is.
#[allow(improper_ctypes_definitions)]
pub type Entrypoint = unsafe extern "sysv64" fn(Mmap) -> !;
