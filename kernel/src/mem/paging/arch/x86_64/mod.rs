use crate::mem::{Address, VirtualAddress};

/// Flush the TLB entry for a single page.
pub fn flush_mapping(addr: VirtualAddress) {
    unsafe { asm!("invlpg [{0}]", in(reg) addr.raw()) };
}

/// Flush the entire TLB.
pub fn flush_all_mappings() {
    // Reloading CR3 flushes the TLB.
    unsafe { asm!("mov rax, cr3", "mov cr3, rax", lateout("rax") _) };
}
