//! Nix defined file structures and interoperation with Nix.
use std::ffi::OsStr;
use std::fmt;
use std::path::{Path, PathBuf};

pub mod eval;
pub mod flake_lock;
pub mod flake_output;
pub mod info;
pub mod nixos_options;

pub const DEFAULT_IMPORT_FILE: &str = "default.nix";
pub const FLAKE_FILE: &str = "flake.nix";
pub const FLAKE_LOCK_FILE: &str = "flake.lock";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlakeUrl(String);

impl FlakeUrl {
    pub fn new_path(path: impl AsRef<Path>) -> Self {
        Self(format!("path:{}", path.as_ref().display()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for FlakeUrl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl AsRef<str> for FlakeUrl {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<OsStr> for FlakeUrl {
    fn as_ref(&self) -> &OsStr {
        self.as_str().as_ref()
    }
}

impl From<&'_ Path> for FlakeUrl {
    fn from(path: &'_ Path) -> Self {
        Self::new_path(path)
    }
}

impl From<PathBuf> for FlakeUrl {
    fn from(path: PathBuf) -> Self {
        Self::new_path(path)
    }
}

#[cfg(test)]
mod tests {
    use std::process::Stdio;

    use tokio::process::Command;

    pub(crate) async fn get_nix_system() -> String {
        let output = Command::new("nix")
            .kill_on_drop(true)
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
            // Configures stdout/stderr automatically.
            .output()
            .await
            .expect("Failed to run nix");
        assert!(output.status.success());
        String::from_utf8(output.stdout).expect("Not UTF-8")
    }
}
