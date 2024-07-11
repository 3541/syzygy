use xshell::{cmd, Shell};

pub fn llvm_binary(sh: &Shell, name: &str) -> Option<String> {
    for path in ["", "/opt/llvm/bin/", "/opt/homebrew/opt/llvm/bin/"] {
        if cmd!(sh, "{path}{name} --version")
            .quiet()
            .ignore_stdout()
            .ignore_stderr()
            .run()
            .is_ok()
        {
            return Some(format!("{path}{name}"));
        }
    }

    None
}
