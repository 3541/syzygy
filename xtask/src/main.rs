mod build;
mod run;
mod rustc;
mod target;
mod targets;

use std::{env, fs, path::Path};

use blake2::{Blake2b512, Digest};
use clap::{Parser, Subcommand};
use xshell::Shell;

use rustc::BuildType;
use target::Arch;

#[derive(Subcommand)]
enum Command {
    Build,
    Run,
}

#[derive(Parser)]
struct Args {
    #[arg(short, long, value_enum, default_value_t = Arch::Amd64)]
    arch: Arch,
    #[arg(short, long, value_enum, default_value_t = BuildType::Debug)]
    build_type: BuildType,

    #[command(subcommand)]
    command: Command,
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

fn main() -> Result<()> {
    let args = Args::parse();
    let sh = Shell::new()?;
    let _env = sh.push_env("RUSTC_BOOTSTRAP", "1");

    match args.command {
        Command::Build => build::build(&args, &sh),
        Command::Run => run::run(&args, &sh),
    }?;

    Ok(())
}
