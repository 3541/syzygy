pub mod constants {
    use crate::memory::VirtualAddress;

    pub const KERNEL_BASE: VirtualAddress =
        unsafe { VirtualAddress::new_const(0xFFFFC00000000000) };
}
