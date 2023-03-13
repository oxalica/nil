use std::collections::HashMap;
use std::path::Path;
use std::process::{Command, Stdio};

use anyhow::{ensure, Context, Result};
use serde::{de, Deserialize};
use syntax::semantic::escape_string;

pub fn eval_all_options(nix_command: &Path, nixpkgs_path: &Path) -> Result<NixosOptions> {
    let nixpkgs_path = nixpkgs_path
        .to_str()
        .with_context(|| format!("Invalid path to nixpkgs: {}", nixpkgs_path.display()))?;

    let output = Command::new(nix_command)
        .args([
            "eval",
            "--experimental-features",
            "nix-command",
            "--read-only",
            "--impure",
            "--json",
            "--show-trace",
            "--expr",
            &escape_string(nixpkgs_path),
            // Workaround: `--argstr` is broken currently.
            // https://github.com/NixOS/nix/issues/2678
            "--apply",
            include_str!("./nixos_options.nix"),
        ])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context("Failed to spawn `nix`")?;

    ensure!(
        output.status.success(),
        "Nix eval failed with {}. Stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );

    let val = serde_json::from_slice(&output.stdout)?;
    Ok(val)
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NixosOptions {
    pub description: Option<Doc>,
    #[serde(default)]
    pub declarations: Vec<String>,
    #[serde(default)]
    pub read_only: bool,
    #[serde(rename = "type")]
    pub ty: Option<String>,
    pub default: Option<Value>,
    pub example: Option<Value>,
    #[serde(default)]
    pub related_packages: Vec<RelatedPackage>,

    #[serde(default)]
    pub children: HashMap<String, NixosOptions>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(tag = "_type")]
pub enum Doc {
    #[serde(rename = "mdDoc")]
    Markdown { text: String },
    #[serde(other)]
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(tag = "_type")]
pub enum Value {
    #[serde(rename = "literalExpression")]
    Expression { text: String },
    #[serde(rename = "literalMD")]
    Markdown { text: String },
    #[serde(other)]
    Other,
}

// https://github.com/NixOS/nixpkgs/blob/28c1aac72e3aef70b8c898ea9c16d5907f9eae22/nixos/lib/make-options-doc/default.nix#L61
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelatedPackage {
    pub path: Vec<String>,
    pub comment: Option<String>,
}

impl<'de> de::Deserialize<'de> for RelatedPackage {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Repr {
            Name(String),
            Path(Vec<String>),
            Full {
                name: Option<String>,
                path: Option<Vec<String>>,
                comment: Option<String>,
            },
        }

        Ok(match Repr::deserialize(deserializer)? {
            Repr::Name(name) => Self {
                path: vec![name],
                comment: None,
            },
            Repr::Path(path) => Self {
                path,
                comment: None,
            },
            Repr::Full {
                name,
                path,
                comment,
            } => Self {
                path: path.or_else(|| Some(vec![name?])).unwrap_or_default(),
                comment,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "requires using 'nix' and 'nixpkgs'"]
    fn nixos_options() {
        let output = Command::new("nix")
            .args([
                "eval",
                "--experimental-features",
                "nix-command",
                "--impure",
                "--expr",
                "<nixpkgs>",
            ])
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .output()
            .unwrap();
        assert!(output.status.success());
        let nixpkgs_path = String::from_utf8(output.stdout).unwrap();
        let opts = eval_all_options("nix".as_ref(), nixpkgs_path.trim().as_ref()).unwrap();

        // Sanity check.
        assert_eq!(
            opts.children["nix"].children["enable"].ty.as_deref(),
            Some("boolean"),
        );
    }
}
