// x86_64 interrupts.

#[inline(always)]
pub fn cli() {
    unsafe { llvm_asm!("cli" :::: "volatile") }
}

#[inline(always)]
pub fn sti() {
    unsafe { llvm_asm!("sti" :::: "volatile") }
}

pub fn interrupts_enabled() -> bool {
    let flags: u64;
    unsafe { asm!("pushfq", "pop {}", out(reg) flags) };

    flags & (1 << 9) != 0
}
