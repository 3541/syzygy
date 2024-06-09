use std::{ops::RangeInclusive, path::PathBuf};

use xshell::{cmd, Shell};

use crate::{
    rustc::{Compile, Config},
    target::Target,
    Result,
};

fn find_linker(sh: &Shell) -> Result<String> {
    const VERSIONS: RangeInclusive<u32> = 13..=18;

    if cmd!(sh, "lld-link --version")
        .quiet()
        .ignore_stdout()
        .run()
        .is_ok()
    {
        return Ok("lld-link".into());
    }

    for ver in VERSIONS.rev() {
        let vs = format!("{ver}");

        if cmd!(sh, "lld-link-{vs} --version")
            .quiet()
            .ignore_stdout()
            .run()
            .is_ok()
        {
            return Ok(format!("lld-link-{ver}"));
        }
    }

    Err("Unable to locate lld-link.".into())
}

pub fn build(sh: &Shell, config: &Config, image_hash: &str) -> Result<PathBuf> {
    let target = Target::efi(config.arch, "sz-efi");
    let linker = find_linker(sh)?;
    let _env = sh.push_env("SZ_KERNEL_HASH", image_hash);

    let c = Compile {
        target,
        build_type: config.build_type,
        extra_args: vec!["-C".into(), format!("linker={linker}")],
    };

    c.build(sh)
}
