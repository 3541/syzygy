use crate::arch::register;
use crate::task::CpuState;

pub struct X86_64CpuState {
    rbx: u64,
    r12: u64,
    r13: u64,
    r14: u64,
    r15: u64,
    rbp: u64,
    fs_base_high: u32,
    fs_base_low: u32,
}

impl X86_64CpuState {
    pub fn set_fs_base(&mut self, fs_base: u64) {
        self.fs_base_high = (fs_base >> 32) as u32;
        self.fs_base_low = fs_base as u32;
    }
}

unsafe impl CpuState for X86_64CpuState {
    #[inline(always)]
    unsafe fn save(&mut self) {
        asm!("mov {}, rbp", out(reg) self.rbp, out("rbx") self.rbx, out("r12") self.r12, out("r13") self.r13, out("r14") self.r14, out("r15") self.r15);
        asm!("rdmsr", in("ecx") register::msr::FS_BASE, out("edx") self.fs_base_high, out("eax") self.fs_base_low);
    }

    #[inline(always)]
    unsafe fn restore(&self) {
        asm!("mov rbp, {}", in(reg) self.rbp, in("rbx") self.rbx, in("r12") self.r12, in("r13") self.r13, in("r14") self.r14, in("r15") self.r15);
        asm!("wrmsr", in("ecx") register::msr::FS_BASE, in("edx") self.fs_base_high, in("eax") self.fs_base_low);
    }

    fn empty() -> X86_64CpuState {
        X86_64CpuState {
            rbx: 0,
            r12: 0,
            r13: 0,
            r14: 0,
            r15: 0,
            rbp: 0,
            fs_base_high: 0,
            fs_base_low: 0,
        }
    }
}
