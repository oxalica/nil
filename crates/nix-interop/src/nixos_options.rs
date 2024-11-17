use std::collections::HashMap;
use std::path::Path;
use std::process::Stdio;

use anyhow::{ensure, Context, Result};
use serde::{de, Deserialize};
use syntax::semantic::escape_string;
use tokio::process::Command;

/// Load the nixos options.
pub async fn eval_all_options(nix_command: &Path, nixpkgs_path: &Path) -> Result<NixosOptions> {
    let nixpkgs_path = nixpkgs_path
        .to_str()
        .filter(|path| path.starts_with('/'))
        .with_context(|| format!("Invalid path to nixpkgs: {}", nixpkgs_path.display()))?;

    let output = Command::new(nix_command)
        .kill_on_drop(true)
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
        // Configures stdout/stderr automatically.
        .output()
        .await
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

pub type NixosOptions = HashMap<String, NixosOption>;

#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NixosOption {
    pub description: Option<Doc>,
    #[serde(default)]
    pub declarations: Vec<String>,
    #[serde(default)]
    pub read_only: bool,
    #[serde(rename = "type")]
    pub ty: Ty,
    pub default: Option<Value>,
    pub example: Option<Value>,
    #[serde(default)]
    pub related_packages: Vec<RelatedPackage>,
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

/// Raw nix types.
#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase", tag = "name")]
pub enum Ty {
    #[default]
    Any,
    Bool,
    Int,
    Float,
    String,
    Path,
    Derivation,
    List {
        elem: Box<Ty>,
    },
    Lambda {
        from: Box<Ty>,
        to: Box<Ty>,
    },
    Attrset {
        #[serde(default)]
        fields: NixosOptions,
        rest: Option<Box<Ty>>,
    },
}

#[cfg(test)]
mod tests {
    use tokio::sync::OnceCell;

    use crate::FlakeUrl;

    use super::*;

    async fn check_nixpkgs(name: &str) {
        static LOCKED: OnceCell<serde_json::Value> = OnceCell::const_new();

        let nixpkgs_path = LOCKED
            .get_or_init(|| async {
                let output = Command::new("nix")
                    .kill_on_drop(true)
                    .args([
                        "flake",
                        "archive",
                        "--experimental-features",
                        "nix-command flakes",
                        "--json",
                    ])
                    .arg(FlakeUrl::new_path("./tests/nixpkgs_revs"))
                    .stdin(Stdio::null())
                    // Configures stdout/stderr automatically.
                    .output()
                    .await
                    .unwrap();
                serde_json::from_slice::<serde_json::Value>(&output.stdout).unwrap()
            })
            .await
            .pointer(&format!("/inputs/{name}/path"))
            .unwrap()
            .as_str()
            .unwrap()
            .to_owned();

        let opts = eval_all_options("nix".as_ref(), nixpkgs_path.as_ref())
            .await
            .unwrap();

        // Sanity check.
        let Ty::Attrset { fields, .. } = &opts["nix"].ty else {
            panic!("Invalid options: {opts:?}");
        };
        let opt = &fields["enable"];
        assert_eq!(opt.ty, Ty::Bool);
        assert!(matches!(
            &opt.description,
            Some(Doc::Markdown { text }) if text.starts_with("Whether to enable Nix.")
        ));
    }

    #[tokio::test]
    #[ignore = "requires using 'nix' and network"]
    async fn nixos_unstable() {
        check_nixpkgs("nixos-unstable").await;
    }

    #[tokio::test]
    #[ignore = "requires using 'nix' and '<nixpkgs>'"]
    async fn nixos_22_11() {
        check_nixpkgs("nixos-22-11").await;
    }

    #[tokio::test]
    #[ignore = "requires using 'nix' and '<nixpkgs>'"]
    async fn nixos_23_05() {
        check_nixpkgs("nixos-23-05").await;
    }
}
