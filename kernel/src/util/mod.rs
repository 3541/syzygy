pub mod sync;

pub fn abort() -> ! {
    unsafe { asm!("ud2") };
    unreachable!()
}
