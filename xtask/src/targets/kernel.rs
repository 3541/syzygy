use std::path::PathBuf;

use xshell::Shell;

use crate::{
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

    c.build(sh)
}
