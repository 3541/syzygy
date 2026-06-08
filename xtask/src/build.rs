use xshell::Shell;

use crate::{Result, rustc::Config, targets};

pub fn build(args: &crate::Args, sh: &Shell) -> Result<()> {
    targets::build(sh, &Config::from(args))?;
    Ok(())
}
