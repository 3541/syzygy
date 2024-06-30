use std::{fmt, path::PathBuf};

use clap::ValueEnum;

use crate::repo_root;

#[derive(Clone, Copy, ValueEnum)]
pub enum Arch {
    Amd64,
    Aarch64,
}

impl fmt::Display for Arch {
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

impl Arch {
    pub fn qemu_arch(&self) -> &'static str {
        match self {
            Self::Amd64 => "x86_64",
            Self::Aarch64 => "aarch64",
        }
    }

    fn rust_arch(&self) -> &'static str {
        self.qemu_arch()
    }

    pub fn qemu_machine(&self) -> &'static str {
        match self {
            Self::Amd64 => "q35",
            Self::Aarch64 => "virt",
        }
    }

    pub fn qemu_cpu(&self) -> &'static str {
        match self {
            Self::Amd64 => "Haswell-v4",
            Self::Aarch64 => todo!(),
        }
    }

    pub fn efi_filename(&self) -> &'static str {
        match self {
            Self::Amd64 => "bootx64.efi",
            Self::Aarch64 => "bootaa64.efi",
        }
    }

    pub fn ld_machine(&self) -> &'static str {
        match self {
            Self::Amd64 => "elf_x86_64",
            Self::Aarch64 => todo!(),
        }
    }
}

pub struct Target {
    arch: Arch,
    triple_suffix: &'static str,
    name: &'static str,
    filename: String,
}

impl Target {
    pub fn efi(arch: Arch, name: &'static str) -> Self {
        Self {
            arch,
            triple_suffix: "unknown-uefi",
            name,
            filename: format!("{}.efi", name),
        }
    }

    pub fn baremetal(arch: Arch, name: &'static str) -> Self {
        Self {
            arch,
            triple_suffix: "unknown-none",
            name,
            filename: format!("lib{}.a", name.replace('-', "_")),
        }
    }

    pub fn triple(&self) -> String {
        format!("{}-{}", self.arch.rust_arch(), self.triple_suffix)
    }

    pub fn out(&self) -> PathBuf {
        repo_root().join("target").join(self.triple())
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn name(&self) -> &'static str {
        self.name
    }
}
