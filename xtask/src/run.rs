use xshell::{cmd, Shell};

use crate::{repo_root, rustc::Config, targets, Result};

pub fn run(args: &crate::Args, sh: &Shell) -> Result<()> {
    let config = Config {
        arch: args.arch,
        build_type: args.build_type,
    };
    let esp = targets::build(sh, &config)?;

    let bin_dir = repo_root().join(format!("boot/efi/bin/{}", config.arch));
    let ovmf = bin_dir.join("OVMF.fd");
    let ovmf_vars = bin_dir.join("OVMF_VARS.fd");
    let qemu_arch = config.arch.qemu_arch();
    let machine = config.arch.qemu_machine();
    cmd!(
        sh,
        "qemu-system-{qemu_arch} -machine {machine} -drive if=pflash,format=raw,readonly=on,file={ovmf} -drive if=pflash,format=raw,readonly=on,file={ovmf_vars} -drive format=raw,file=fat:rw:{esp}"
    )
    .run()?;

    Ok(())
}
