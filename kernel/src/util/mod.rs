pub mod arch;
pub mod either;
pub mod sync;

pub use arch::register;

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
