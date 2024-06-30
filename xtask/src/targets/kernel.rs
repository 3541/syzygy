use std::path::PathBuf;

use xshell::{cmd, Shell};

use crate::{
    repo_root,
    rustc::{Compile, Config},
    target::Target,
    Result,
};

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

    cmd!(
        sh,
        "ld -m{machine} --static --nostdlib --as-needed --gc-sections -z max-page-size=0x1000 --pie -T {ldscript} -o {out} {lib}"
    ).run()?;

    Ok(out)
}
