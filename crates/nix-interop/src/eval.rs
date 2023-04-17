//! Wrapper for `nix eval`.
use std::path::Path;
use std::process::Stdio;

use anyhow::{ensure, Context, Result};
use serde::de::DeserializeOwned;
use tokio::process::Command;

pub async fn nix_eval_expr_json<T: DeserializeOwned>(nix_command: &Path, expr: &str) -> Result<T> {
    let output = Command::new(nix_command)
        .kill_on_drop(true)
        .args([
            "eval",
            "--experimental-features",
            "nix-command",
            "--read-only",
            "--json",
            "--expr",
            expr,
        ])
        .stdin(Stdio::null())
        // Configures stdout/stderr automatically.
        .output()
        .await
        .with_context(|| format!("Failed to spawn {nix_command:?}"))?;

    ensure!(
        output.status.success(),
        "Nix eval failed with {}.\nExpression: {}\nStderr: {}",
        output.status,
        expr,
        String::from_utf8_lossy(&output.stderr),
    );

    let val = serde_json::from_slice(&output.stdout)?;
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore = "requires calling 'nix'"]
    async fn nix_eval_simple() {
        let ret = nix_eval_expr_json::<i64>("nix".as_ref(), "1 + 1")
            .await
            .unwrap();
        assert_eq!(ret, 2);
    }

    #[tokio::test]
    #[ignore = "requires calling 'nix'"]
    async fn nix_eval_error() {
        nix_eval_expr_json::<i64>("nix".as_ref(), "{ }.not-exist")
            .await
            .unwrap_err();
    }
}
