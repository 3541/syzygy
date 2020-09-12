#[inline(always)]
pub fn pause() {
    unsafe { asm!("pause") }
}
