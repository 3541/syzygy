use logc::{debug, warn};

use super::{InterruptVector, LvtFlags};
use crate::arch::{self, cpuid, register, PrivilegeLevel};
use crate::interrupt::idt;
use crate::interrupt::timer::read_tsc;
use crate::VirtualAddress;

pub enum ApicTimerMode {
    Oneshot,
    TscDeadline,
}

pub struct ApicTimer {
    lvt_register: VirtualAddress,
    mode: ApicTimerMode,
}

impl ApicTimer {
    pub fn new(lvt_register: VirtualAddress) -> ApicTimer {
        let mode = if cpuid::has_tsc_deadline() {
            if !cpuid::has_invariant_tsc() {
                warn!("Using APIC timer in TSC deadline mode without invariant TSC. Expect timer issues.");
            }
            debug!("CPU supports TSC deadline mode.");
            ApicTimerMode::TscDeadline
        } else {
            debug!("CPU does not support TSC deadline mode. Using oneshot.");
            ApicTimerMode::Oneshot
        };

        let mut ret = ApicTimer { lvt_register, mode };

        match ret.mode {
            ApicTimerMode::TscDeadline => {
                unsafe {
                    ret.lvt_register.as_mut_ptr::<u32>().write_volatile(
                        (LvtFlags::DELIVERY_MODE_FIXED | LvtFlags::TIMER_MODE_TSC_DEADLINE).bits()
                            | InterruptVector::Timer as u32,
                    );
                }
                arch::mfence();

                idt().set_handler(
                    InterruptVector::Timer,
                    ApicTimer::tsc_deadline_handler,
                    PrivilegeLevel::User,
                );
            }
            ApicTimerMode::Oneshot => warn!("Oneshot timer is not yet implemented."),
        }

        ret
    }

    pub unsafe fn arm(&mut self, delay: u64) {
        match self.mode {
            ApicTimerMode::TscDeadline => register::write::tsc_deadline(read_tsc() + delay),
            ApicTimerMode::Oneshot => warn!("Oneshot timer is not yet implemented."),
        }
    }

    irq_handler!(InterruptVector::Timer => fn tsc_deadline_handler(_stack) {
        if read_tsc() < register::read::tsc_deadline() {
            warn!("Spurious TSC deadline interrupt. Ignoring.");
            return;
        }

        unsafe { register::write::tsc_deadline(read_tsc() + 1_000_000_000) };
    });
}
