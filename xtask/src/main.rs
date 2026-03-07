mod build;
mod find;
mod run;
mod rustc;
mod target;
mod targets;

use std::{env::consts::ARCH, fs, path::Path};

use blake2::{Blake2b512, Digest};
use clap::{Parser, Subcommand};
use xshell::{Shell, cmd};

use rustc::BuildType;
use target::Arch;

#[derive(Subcommand)]
enum Command {
    Build,
    Run {
        #[arg(long)]
        force_emu: bool,
    },
}

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    arch: Option<Arch>,
    #[arg(short, long, value_enum, default_value_t = BuildType::Debug)]
    build_type: BuildType,

    #[command(subcommand)]
    command: Command,
}

impl Args {
    fn arch(&self) -> Arch {
        self.arch
            .or(Arch::try_from(ARCH).ok())
            .unwrap_or(Arch::Amd64)
    }
}

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(1)
        .unwrap()
}

fn hash(path: &Path) -> Result<String> {
    let data = fs::read(path)?;
    let mut h = Blake2b512::new();
    h.update(&data);
    Ok(format!("{:x}", h.finalize()))
}

fn version(sh: &Shell) -> Result<String> {
    let hash = cmd!(sh, "git rev-parse --short HEAD").quiet().read()?;
    let tag = env!("CARGO_PKG_VERSION");
    let tag_hash = cmd!(sh, "git rev-parse --short {tag}")
        .quiet()
        .ignore_stderr()
        .read()
        .unwrap_or("<no tag>".into());

    if hash == tag_hash {
        return Ok(tag.into());
    }
    if cmd!(sh, "git diff-index --quiet HEAD")
        .quiet()
        .ignore_stdout()
        .run()
        .is_ok()
    {
        Ok(format!("{tag}-{hash}"))
    } else {
        Ok(format!("{tag}-{hash}*"))
    }
}

fn main() -> Result<()> {
    let args = Args::parse();
    let sh = Shell::new()?;
    let _env = vec![
        sh.push_env("RUSTC_BOOTSTRAP", "1"),
        sh.push_env("SZ_VER", version(&sh)?),
    ];

    match args.command {
        Command::Build => build::build(&args, &sh),
        Command::Run { .. } => run::run(&args, &sh),
    }?;

    Ok(())
}
