//! Nix defined file structures and interoperation with Nix.
pub mod eval;
pub mod flake_lock;
pub mod flake_output;
pub mod info;
pub mod nixos_options;

pub const DEFAULT_IMPORT_FILE: &str = "default.nix";
pub const FLAKE_FILE: &str = "flake.nix";
pub const FLAKE_LOCK_FILE: &str = "flake.lock";

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
