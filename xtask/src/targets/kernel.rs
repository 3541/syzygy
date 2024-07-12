use std::path::PathBuf;

use xshell::{cmd, Shell};

use crate::{
    repo_root,
    rustc::{Compile, Config},
    target::Target,
    Result,
};

#[cfg(any(target_os = "macos", target_os = "windows"))]
use crate::find::llvm_binary;

#[allow(unreachable_code, unused_variables)]
fn linker(sh: &Shell) -> Result<String> {
    // ld on Darwin is ld64, which only produces Mach-O objects. Windows is even more of a special
    // snowflake and does not provide anything named "ld" (if it did, it would be link.exe, which
    // would be similarly unsuitable).
    #[cfg(any(target_os = "macos", target_os = "windows"))]
    return llvm_binary(sh, "ld.lld")
        .ok_or("lld is required to produce ELF objects on this platform".into());

    // Otherwise, hope the system linker exists and is sensible.
    Ok("ld".into())
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
