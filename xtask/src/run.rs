use xshell::{Shell, cmd};

use crate::{Result, repo_root, rustc::Config, targets};

pub fn run(args: &crate::Args, sh: &Shell) -> Result<()> {
    let crate::Command::Run { force_emu } = args.command else {
        panic!("what?");
    };

    let config = Config::from(args);
    let esp = targets::build(sh, &config)?;

    let bin_dir = repo_root().join(format!("boot/efi/bin/{}", config.arch));
    let ovmf = bin_dir.join("OVMF.fd");
    let ovmf_vars = bin_dir.join("OVMF_VARS.fd");
    let qemu_arch = config.arch.qemu_arch();
    let machine = config.arch.qemu_machine();
    let cpu = if config.arch.is_host() && !force_emu { "host"} else { config.arch.qemu_cpu() };

    let accel = if force_emu { "tcg" } else { "kvm:hvf:tcg" };

    cmd!(
        sh,
        "qemu-system-{qemu_arch} -machine {machine},accel={accel} -cpu {cpu} -drive if=pflash,format=raw,readonly=on,file={ovmf} -drive if=pflash,format=raw,readonly=on,file={ovmf_vars} -drive format=raw,file=fat:rw:{esp} -nographic"
    )
    .run()?;

    Ok(())
}
