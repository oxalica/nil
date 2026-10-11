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

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("Nix exceeds memory limit")]
pub struct NixOutOfMemory;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlakeUrl(String);

impl FlakeUrl {
    pub fn new_path(path: impl AsRef<Path>) -> Self {
        Self(format!("path:{}", path.as_ref().display()))
    }

    /// The Nix CLI resolves a local flake inside a Git work tree to `git+file:`,
    /// which copies only tracked files.
    pub fn new_local(path: impl AsRef<Path>) -> Self {
        let path = path.as_ref();
        let Some(top) = path.ancestors().find(|dir| dir.join(".git").exists()) else {
            return Self::new_path(path);
        };
        let subdir = path.strip_prefix(top).expect("ancestors are prefixes");
        if subdir.as_os_str().is_empty() {
            Self(format!("git+file://{}", top.display()))
        } else {
            Self(format!(
                "git+file://{}?dir={}",
                top.display(),
                subdir.display()
            ))
        }
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
    use std::fs;
    use std::process::Stdio;

    use tokio::process::Command;

    use super::FlakeUrl;

    pub(crate) fn scratch_dir(name: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!("nil-{name}-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn local_flake_url() {
        let root = scratch_dir("local-flake-url");

        let plain = root.join("plain");
        fs::create_dir_all(&plain).unwrap();
        assert_eq!(
            FlakeUrl::new_local(&plain).as_str(),
            format!("path:{}", plain.display()),
        );

        let repo = root.join("repo");
        let nested = repo.join("sub/flake");
        fs::create_dir_all(repo.join(".git")).unwrap();
        fs::create_dir_all(&nested).unwrap();
        assert_eq!(
            FlakeUrl::new_local(&repo).as_str(),
            format!("git+file://{}", repo.display()),
        );
        assert_eq!(
            FlakeUrl::new_local(&nested).as_str(),
            format!("git+file://{}?dir=sub/flake", repo.display()),
        );

        let worktree = root.join("worktree");
        fs::create_dir_all(&worktree).unwrap();
        fs::write(worktree.join(".git"), "gitdir: ../repo/.git/worktrees/w\n").unwrap();
        assert_eq!(
            FlakeUrl::new_local(&worktree).as_str(),
            format!("git+file://{}", worktree.display()),
        );

        fs::remove_dir_all(&root).unwrap();
    }

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
