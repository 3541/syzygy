use getrandom::{register_custom_getrandom, Error};

use core::{arch::asm, cmp::min, mem::size_of};

const FEAT_RNG: u64 = 0b1111 << 60;

fn rand(buf: &mut [u8]) -> Result<(), Error> {
    assert!(buf.len() > 0);

    let mut features: u64;
    unsafe { asm!("mrs {}, ID_AA64ISAR0_EL1", out(reg) features) };

    if features & FEAT_RNG == 0 {
        panic!("FEAT_RNG not supported.");
    }

    for i in 0..=buf.len() / size_of::<u64>() {
        let res: u64;
        let pstate: u64;
        unsafe {
            asm!(
                "mrs {res}, s3_3_c2_c4_1",
                "mrs {pstate}, nzcv",
                res = out(reg) res,
                pstate = out(reg) pstate,
                options(nomem, nostack)
            )
        };

        if res == 0 && pstate == 0b0000 {
            panic!("RNDRSS failed.");
        }

        let offset = i * size_of::<u64>();
        let len = min(size_of::<u64>(), buf.len() - offset);
        buf[offset..offset + len].copy_from_slice(&res.to_ne_bytes()[..len]);
    }

    Ok(())
}

register_custom_getrandom!(rand);
