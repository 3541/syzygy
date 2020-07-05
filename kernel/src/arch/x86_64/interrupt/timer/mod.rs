pub use super::apic::timer as apic_timer;

pub use apic_timer::ApicTimer;

pub fn read_tsc() -> u64 {
    let low: u64;
    let high: u64;

    unsafe { asm!("rdtsc", out("eax") low, out("edx") high) };
    low | (high << 32)
}
