use std::{ops::RangeInclusive, path::PathBuf};

use xshell::Shell;

use crate::{
    find::llvm_binary,
    rustc::{Compile, Config},
    target::Target,
    Result,
};

fn find_linker(sh: &Shell) -> Result<String> {
    const VERSIONS: RangeInclusive<u32> = 13..=18;

    if let Some(p) = llvm_binary(sh, "lld-link") {
        return Ok(p);
    }

    for ver in VERSIONS.rev() {
        if let Some(p) = llvm_binary(sh, &format!("lld-link-{ver}")) {
            return Ok(p);
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
        alloc: false,
    };

    c.build(sh)
}
