//! Parser for the flake lock file.
//!
//! We want a custom `nix flake archive` without dumping the current flake
//! which may be very costly for large repositories like nixpkgs.
//!
//! https://github.com/NixOS/nix/blob/2.13.1/src/nix/flake.md#lock-files
use std::collections::HashMap;
use std::path::Path;

use anyhow::{bail, ensure, Context, Result};
use serde::Deserialize;
use serde_repr::Deserialize_repr;

use crate::eval::nix_eval_expr_json;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedInput {
    pub store_path: String,
    pub is_flake: bool,
}

/// Resolve all root inputs from a flake lock.
pub fn resolve_flake_locked_inputs(
    nix_command: &Path,
    lock_src: &[u8],
) -> Result<HashMap<String, ResolvedInput>> {
    let lock =
        serde_json::from_slice::<FlakeLock>(lock_src).context("Failed to parse flake lock")?;
    let root_node = lock.nodes.get(&lock.root).context("Missing root node")?;

    // Resolve followed inputs.
    let inputs = root_node
        .inputs
        .iter()
        .map(|(input_name, input)| {
            let name_seq = match input {
                FlakeInput::Node(name) => std::slice::from_ref(name),
                FlakeInput::Follow(name_seq) => name_seq,
            };
            let target = name_seq.iter().try_fold(root_node, |node, input| {
                match node
                    .inputs
                    .get(input)
                    .with_context(|| format!("Missing followed input {input:?}"))?
                {
                    FlakeInput::Node(name) => lock
                        .nodes
                        .get(name)
                        .with_context(|| format!("Missing followed node {name:?}")),
                    FlakeInput::Follow(_) => bail!("Chained 'follows' is not supported"),
                }
            })?;

            let nar_hash = &target
                .locked
                .as_ref()
                .with_context(|| format!("Flake input {input_name:?} is not locked"))?
                .nar_hash;

            // Validate since we'll wrap this in Nix strings below.
            ensure!(
                nar_hash.bytes().all(|b| b != b'\\' && b != b'"'),
                "Invalid nar hash"
            );

            Ok((input_name, target.flake, nar_hash))
        })
        .collect::<Result<Vec<_>>>()?;

    // Trivial case to skip calling Nix.
    if inputs.is_empty() {
        return Ok(HashMap::new());
    }

    let hashes = inputs
        .iter()
        .flat_map(|(_, _, hash)| ["\"", hash, "\" "])
        .collect::<String>();
    let store_paths = nix_eval_expr_json::<Vec<String>>(
        nix_command,
        &format!(
            r#"
            builtins.map (hash: (derivation {{
                name = "source";
                system = "dummy";
                builder = "dummy";
                outputHashMode = "recursive";
                outputHashAlgo = null;
                outputHash = hash;
            }}).outPath) [ {hashes} ]
            "#
        ),
    )?;

    let resolved = std::iter::zip(inputs, store_paths)
        .map(|((name, is_flake, _), store_path)| {
            (
                name.to_owned(),
                ResolvedInput {
                    is_flake,
                    store_path,
                },
            )
        })
        .collect();
    Ok(resolved)
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct FlakeLock {
    version: Version,
    root: String,
    nodes: HashMap<String, FlakeNode>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize_repr)]
#[repr(u8)]
enum Version {
    V7 = 7,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct FlakeNode {
    #[serde(default)]
    inputs: HashMap<String, FlakeInput>,
    /// For the root node (the current flake), this is `None`.
    locked: Option<LockedFlakeRef>,
    #[serde(default = "const_true")]
    flake: bool,
}

fn const_true() -> bool {
    true
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
enum FlakeInput {
    Node(String),
    Follow(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LockedFlakeRef {
    nar_hash: String,
    // ...
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "requires calling 'nix'"]
    fn test_resolve_flake_lock_inputs() {
        // {
        //   inputs.nixpkgs.url = "github:NixOS/nixpkgs/5ed481943351e9fd354aeb557679624224de38d5";
        //   inputs.flake-utils = {
        //     url = "github:numtide/flake-utils/5aed5285a952e0b949eb3ba02c12fa4fcfef535f";
        //     flake = false;
        //   };
        //   outputs = { ... }: { };
        // }
        let lock_src = br#"
{
  "nodes": {
    "flake-utils": {
      "flake": false,
      "locked": {
        "lastModified": 1667395993,
        "narHash": "sha256-nuEHfE/LcWyuSWnS8t12N1wc105Qtau+/OdUAjtQ0rA=",
        "owner": "numtide",
        "repo": "flake-utils",
        "rev": "5aed5285a952e0b949eb3ba02c12fa4fcfef535f",
        "type": "github"
      },
      "original": {
        "owner": "numtide",
        "repo": "flake-utils",
        "rev": "5aed5285a952e0b949eb3ba02c12fa4fcfef535f",
        "type": "github"
      }
    },
    "nixpkgs": {
      "locked": {
        "lastModified": 1674211260,
        "narHash": "sha256-xU6Rv9sgnwaWK7tgCPadV6HhI2Y/fl4lKxJoG2+m9qs=",
        "owner": "NixOS",
        "repo": "nixpkgs",
        "rev": "5ed481943351e9fd354aeb557679624224de38d5",
        "type": "github"
      },
      "original": {
        "owner": "NixOS",
        "repo": "nixpkgs",
        "rev": "5ed481943351e9fd354aeb557679624224de38d5",
        "type": "github"
      }
    },
    "root": {
      "inputs": {
        "flake-utils": "flake-utils",
        "nixpkgs": "nixpkgs"
      }
    }
  },
  "root": "root",
  "version": 7
}
        "#;
        let got = resolve_flake_locked_inputs("nix".as_ref(), lock_src).unwrap();
        let expect = HashMap::from_iter([
            (
                "nixpkgs".to_owned(),
                ResolvedInput {
                    store_path: "/nix/store/hap5a6iw5rccl21adfxh5b3lk2c8qnmj-source".to_owned(),
                    is_flake: true,
                },
            ),
            (
                "flake-utils".to_owned(),
                ResolvedInput {
                    store_path: "/nix/store/sk4ga2wy0b02k7pnzakwq4r3jdknda4g-source".to_owned(),
                    is_flake: false,
                },
            ),
        ]);
        assert_eq!(got, expect);
    }
}
