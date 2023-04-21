use std::collections::HashMap;
use std::path::Path;
use std::process::Stdio;

use anyhow::{ensure, Context, Result};
use serde::Deserialize;
use tokio::process::Command;

pub async fn eval_flake_output(
    nix_command: &Path,
    flake_path: &Path,
    legacy: bool,
) -> Result<FlakeOutput> {
    let output = Command::new(nix_command)
        .kill_on_drop(true)
        .args([
            "flake",
            "show",
            "--experimental-features",
            "nix-command flakes",
            "--json",
        ])
        .args(legacy.then_some("--legacy"))
        .arg(flake_path)
        .stdin(Stdio::null())
        // Configures stdout/stderr automatically.
        .output()
        .await
        .context("Failed to spawn `nix`")?;

    ensure!(
        output.status.success(),
        "`nix flake show` failed with {}. Stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );

    let val = serde_json::from_slice(&output.stdout)?;
    Ok(val)
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
pub enum FlakeOutput {
    Leaf(Leaf),
    Attrset(HashMap<String, FlakeOutput>),
}

impl FlakeOutput {
    pub fn as_leaf(&self) -> Option<&Leaf> {
        match self {
            Self::Leaf(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_attrset(&self) -> Option<&HashMap<String, FlakeOutput>> {
        match self {
            Self::Attrset(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Leaf {
    #[serde(rename = "type")]
    pub type_: Type,
    pub name: Option<String>,
    pub description: Option<String>,
}

// https://github.com/NixOS/nix/blob/2.14.1/src/nix/flake.cc#L1105
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Type {
    NixosModule,
    Derivation,
    #[serde(other)]
    Unknown,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore = "requires calling 'nix'"]
    async fn self_() {
        let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let output = eval_flake_output("nix".as_ref(), dir.as_ref(), false)
            .await
            .unwrap();
        let system = crate::tests::get_nix_system().await;
        let leaf = (|| {
            output.as_attrset()?["packages"].as_attrset()?[&system].as_attrset()?["nil"].as_leaf()
        })()
        .unwrap();
        assert_eq!(leaf.type_, Type::Derivation);
        assert!(leaf.name.as_ref().unwrap().starts_with("nil-unstable-"));
        assert!(leaf
            .description
            .as_ref()
            .unwrap()
            .to_lowercase()
            .contains("language server"));
    }
}
