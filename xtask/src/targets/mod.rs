mod efi;
mod esp;
mod kernel;

use std::path::PathBuf;

use xshell::Shell;

use crate::{hash, rustc::Config, Result};

pub fn build(sh: &Shell, config: &Config) -> Result<PathBuf> {
    let kernel = kernel::build(&sh, &config)?;
    let efi = efi::build(&sh, &config, &hash(&kernel)?)?;
    esp::build(sh, config, &kernel, &efi)
}
