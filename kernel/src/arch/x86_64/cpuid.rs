use spin::Once;

static SUPPORT_BOUND: Once<u32> = Once::new();

#[repr(u32)]
enum CpuidLeaf {
    Features = 1,
    HighestRequest = 0x8000_0000,
    AdvancedPowerManagementInfo = 0x8000_0007,
}

fn highest_request() -> u32 {
    *SUPPORT_BOUND.call_once(|| {
        let eax;
        unsafe { asm!("cpuid", inlateout("eax") CpuidLeaf::HighestRequest as u32 => eax, out("ebx") _, out("ecx") _, out("edx") _) };
        eax
    })
}

fn features() -> Option<(u32, u32)> {
    if highest_request() < CpuidLeaf::Features as u32 {
        None
    } else {
        let ecx;
        let edx;
        unsafe {
            asm!("cpuid", inlateout("eax") CpuidLeaf::Features as u32 => _, out("ebx") _, out("ecx") ecx, out("edx") edx)
        };
        Some((ecx, edx))
    }
}

fn advanced_power_management_info() -> Option<(u32, u32, u32, u32)> {
    if highest_request() < CpuidLeaf::AdvancedPowerManagementInfo as u32 {
        crate::debug!("HERE, hr: 0x{:x}", highest_request());
        None
    } else {
        let eax;
        let ebx;
        let ecx;
        let edx;
        unsafe {
            asm!("cpuid", inlateout("eax") CpuidLeaf::AdvancedPowerManagementInfo as u32 => eax, out("ebx") ebx, out("ecx") ecx, out("edx") edx)
        };
        Some((eax, ebx, ecx, edx))
    }
}

pub fn has_apic() -> bool {
    if let Some((_, edx)) = features() {
        edx & (1 << 9) != 0
    } else {
        false
    }
}

pub fn has_tsc_deadline() -> bool {
    if let Some((ecx, _)) = features() {
        ecx & (1 << 24) != 0
    } else {
        false
    }
}

pub fn has_invariant_tsc() -> bool {
    if let Some((_, _, _, edx)) = advanced_power_management_info() {
        edx & (1 << 8) != 0
    } else {
        false
    }
}
