use core::ops::RangeInclusive;
use std::env;

use xshell::{cmd, Shell};

use crate::{repo_root, Result};

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

pub fn run(args: &crate::Args) -> Result<()> {
    let sh = Shell::new()?;
    let cargo = env!("CARGO");
    let _env = sh.push_env("RUSTC_BOOTSTRAP", "1");

    let triple = args.target.triple();
    let linker = find_linker(&sh)?;
    cmd!(
        sh,
        "{cargo} rustc -p sz-efi --target {triple} -Zbuild-std=core -- -C linker={linker}"
    )
    .run()?;

    let target = repo_root().join("target");
    let esp = target.join("esp");
    sh.remove_path(&esp)?;
    sh.create_dir(esp.join("efi/boot"))?;

    sh.copy_file(
        target.join(triple).join("debug/sz-efi.efi"),
        esp.join(format!("efi/boot/{}", args.target.efi_filename())),
    )?;

    let bin_dir = repo_root().join(format!("boot/efi/bin/{}", args.target));
    let ovmf = bin_dir.join("OVMF.fd");
    let ovmf_vars = bin_dir.join("OVMF_VARS.fd");
    let qemu_arch = args.target.qemu_arch();
    let machine = args.target.qemu_machine();
    cmd!(
        sh,
        "qemu-system-{qemu_arch} -machine {machine} -drive if=pflash,format=raw,readonly=on,file={ovmf} -drive if=pflash,format=raw,readonly=on,file={ovmf_vars} -drive format=raw,file=fat:rw:{esp}"
    )
    .run()?;

    Ok(())
}
