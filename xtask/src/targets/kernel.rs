use std::path::PathBuf;

use xshell::{cmd, Shell};

use crate::{
    find::llvm_binary,
    repo_root,
    rustc::{Compile, Config},
    target::Target,
    Result,
};

fn linker(sh: &Shell) -> Result<String> {
    #[cfg(target_os = "macos")]
    return llvm_binary(sh, "ld.lld").ok_or("lld is required to produce ELF objects".into());

    #[cfg(not(target_os = "macos"))]
    return Ok("ld");
}

pub fn build(sh: &Shell, config: &Config) -> Result<PathBuf> {
    let target = Target::baremetal(config.arch, "sz-kernel");

    let c = Compile {
        target,
        build_type: config.build_type,
        extra_args: vec![],
    };
    let lib = c.build(sh)?;
    let out = lib.parent().unwrap().join("sz");
    let ldscript = repo_root().join(format!("kernel/link/{}.ld", config.arch));
    let machine = config.arch.ld_machine();
    let linker = linker(sh)?;

    cmd!(
        sh,
        "{linker} -m{machine} --static --nostdlib --as-needed --gc-sections -z max-page-size=0x1000 --pie -T {ldscript} -o {out} {lib}"
    ).run()?;

    Ok(out)
}
