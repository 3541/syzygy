use xshell::Shell;

use crate::{rustc::Config, targets, Result};

pub fn build(args: &crate::Args, sh: &Shell) -> Result<()> {
    let config = Config {
        arch: args.arch,
        build_type: args.build_type,
    };

    targets::build(&sh, &config)?;
    Ok(())
}
