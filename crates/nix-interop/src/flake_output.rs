use std::collections::HashMap;
use std::path::Path;
use std::process::{Command, Stdio};

use anyhow::{ensure, Context, Result};
use serde::Deserialize;

pub fn eval_flake_output(nix_command: &Path, flake_path: &Path) -> Result<FlakeOutput> {
    let output = Command::new(nix_command)
        .args([
            "flake",
            "show",
            "--experimental-features",
            "nix-command flakes",
            "--json",
            "--legacy",
        ])
        .arg(flake_path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
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
    use crate::tests::NIX_SYSTEM;

    use super::*;

    #[test]
    #[ignore = "requires calling 'nix'"]
    fn self_() {
        let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let output = eval_flake_output("nix".as_ref(), dir.as_ref()).unwrap();
        let leaf = (|| {
            output.as_attrset()?["packages"].as_attrset()?[&*NIX_SYSTEM].as_attrset()?["nil"]
                .as_leaf()
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
