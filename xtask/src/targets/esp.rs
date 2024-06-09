use std::path::{Path, PathBuf};

use xshell::Shell;

use crate::{repo_root, rustc::Config, Result};

pub fn build(sh: &Shell, config: &Config, kernel: &Path, loader: &Path) -> Result<PathBuf> {
    let esp = repo_root().join(format!("target/esp/{}/{}", config.arch, config.build_type));
    sh.remove_path(&esp)?;

    let boot = esp.join("efi/boot");
    sh.create_dir(&boot)?;

    sh.copy_file(kernel, esp.join("sz"))?;
    sh.copy_file(loader, boot.join(config.arch.efi_filename()))?;

    Ok(esp)
}
