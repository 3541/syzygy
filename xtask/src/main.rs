use std::{env, fmt, path::Path};

use clap::{Parser, Subcommand, ValueEnum};

mod run;

#[derive(Clone, Copy, ValueEnum)]
enum Target {
    Amd64,
    Aarch64,
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Amd64 => "amd64",
                Self::Aarch64 => "aarch64",
            }
        )
    }
}

impl Target {
    fn triple(&self) -> &'static str {
        match self {
            Self::Amd64 => "x86_64-unknown-uefi",
            Self::Aarch64 => "aarch64-unknown-uefi",
        }
    }

    fn qemu_arch(&self) -> &'static str {
        match self {
            Self::Amd64 => "x86_64",
            Self::Aarch64 => "aarch64",
        }
    }

    fn qemu_machine(&self) -> &'static str {
        match self {
            Self::Amd64 => "q35",
            Self::Aarch64 => "virt",
        }
    }

    fn efi_filename(&self) -> &'static str {
        match self {
            Self::Amd64 => "bootx64.efi",
            Self::Aarch64 => "bootaa64.efi",
        }
    }
}

#[derive(Subcommand)]
enum Command {
    Run,
}

#[derive(Parser)]
struct Args {
    #[arg(short, long, value_enum, default_value_t = Target::Amd64)]
    target: Target,

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

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Run => run::run(&args),
    }?;

    Ok(())
}
