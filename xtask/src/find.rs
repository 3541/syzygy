use xshell::{cmd, Shell};

#[cfg(not(target_os = "windows"))]
const SUFFIXES: [&'static str; 1] = [""];

#[cfg(target_os = "windows")]
const SUFFIXES: [&'static str; 2] = ["", ".exe"];

pub fn llvm_binary(sh: &Shell, name: &str) -> Option<String> {
    for path in ["", "/opt/llvm/bin/", "/opt/homebrew/opt/llvm/bin/"] {
        for suffix in SUFFIXES {
            let binary = format!("{path}{name}{suffix}");

            if cmd!(sh, "{binary} --version")
                .quiet()
                .ignore_stdout()
                .ignore_stderr()
                .run()
                .is_ok()
            {
                return Some(binary);
            }
        }
    }

    None
}
