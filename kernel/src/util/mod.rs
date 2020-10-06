pub mod arch;
pub mod sync;

pub fn halt_loop() -> ! {
    loop {
        unsafe { asm!("hlt") };
    }
}

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum PrivilegeLevel {
    Kernel = 0,
    User = 3,
}
