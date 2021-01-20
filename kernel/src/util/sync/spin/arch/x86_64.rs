/// An optimization for busy loops.
#[inline(always)]
pub fn pause() {
    unsafe { asm!("pause") }
}
