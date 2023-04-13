//! Nix defined file structures and interoperation with Nix.
pub mod eval;
pub mod flake_lock;
pub mod flake_output;
pub mod nixos_options;

pub const DEFAULT_IMPORT_FILE: &str = "default.nix";
pub const FLAKE_FILE: &str = "flake.nix";
pub const FLAKE_LOCK_FILE: &str = "flake.lock";

#[cfg(test)]
mod tests {
    use std::process::{Command, Stdio};

    use once_cell::sync::Lazy;

    pub(crate) static NIX_SYSTEM: Lazy<String> = Lazy::new(|| {
        let output = Command::new("nix")
            .args([
                "--experimental-features",
                "nix-command",
                "eval",
                "--impure",
                "--read-only",
                "--raw",
                "--expr",
                "builtins.currentSystem",
            ])
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("Failed to run nix");
        assert!(output.status.success());
        String::from_utf8(output.stdout).expect("Not UTF-8")
    });
}
