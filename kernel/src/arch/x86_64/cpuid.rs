use spin::Once;

static support_bound: Once<u32> = Once::new();

#[repr(u32)]
enum CpuidLeaf {
    HighestRequest,
    Features,
}

fn highest_request() -> u32 {
    *support_bound.call_once(|| {
        let mut eax = CpuidLeaf::HighestRequest as u32;
        unsafe { asm!("cpuid", inout("eax") eax, out("ebx") _, out("ecx") _, out("edx") _) };
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

pub fn has_apic() -> bool {
    if let Some((_, edx)) = features() {
        edx & (1 << 9) != 0
    } else {
        false
    }
}
