use std::{fmt, path::PathBuf};

use clap::ValueEnum;
use xshell::{cmd, Shell};

use crate::{
    target::{Arch, Target},
    Args, Result,
};

#[derive(Clone, Copy, ValueEnum)]
pub enum BuildType {
    Debug,
    Release,
}

impl fmt::Display for BuildType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Debug => "debug",
                Self::Release => "release",
            }
        )
    }
}

pub struct Config {
    pub arch: Arch,
    pub build_type: BuildType,
}

impl From<&Args> for Config {
    fn from(value: &Args) -> Self {
        Self {
            arch: value.arch(),
            build_type: value.build_type,
        }
    }
}

pub struct Compile {
    pub target: Target,
    pub build_type: BuildType,
    pub extra_args: Vec<String>,
    pub alloc: bool,
}

impl Compile {
    const CARGO: &'static str = env!("CARGO");

    pub fn build(&self, sh: &Shell) -> Result<PathBuf> {
        let cargo = Self::CARGO;
        let package = self.target.name();
        let triple = self.target.triple();
        let build_type = match self.build_type {
            BuildType::Debug => "dev",
            BuildType::Release => "release",
        };
        let extra = &self.extra_args;
        let core_libs = if self.alloc { "core,alloc" } else { "core" };

        cmd!(
            sh,
            "{cargo} rustc -p {package} --target {triple} -Zbuild-std={core_libs} --profile {build_type} -- {extra...}"
        )
        .run()?;

        Ok(self
            .target
            .out()
            .join(format!("{}/{}", self.build_type, self.target.filename())))
    }
}
