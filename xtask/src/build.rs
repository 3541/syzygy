use xshell::Shell;

use crate::{rustc::Config, targets, Result};

pub fn build(args: &crate::Args, sh: &Shell) -> Result<()> {
    targets::build(&sh, &Config::from(args))?;
    Ok(())
}
