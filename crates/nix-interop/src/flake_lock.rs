//! Parser for the flake lock file.
//!
//! We want a custom `nix flake archive` without dumping the current flake
//! which may be very costly for large repositories like nixpkgs.
//!
//! <https://github.com/NixOS/nix/blob/2.13.1/src/nix/flake.md#lock-files>
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::process::Stdio;

use anyhow::{ensure, Context, Result};
use serde::Deserialize;
use serde_repr::Deserialize_repr;
use tokio::process::Command;

use crate::eval::nix_eval_expr_json;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedInput {
    pub store_path: String,
    pub is_flake: bool,
}

/// Resolve all root inputs from a flake lock.
pub async fn resolve_flake_locked_inputs(
    nix_command: &Path,
    lock_src: &[u8],
) -> Result<HashMap<String, ResolvedInput>> {
    let lock =
        serde_json::from_slice::<FlakeLock>(lock_src).context("Failed to parse flake lock")?;

    let mut resolver = Resolver::new(&lock);
    let root_node = resolver.get_node(&lock.root).context("Missing root node")?;
    let mut inputs = resolver
        .resolve_node_inputs(&lock.root)
        .context("Failed to resolve inputs from flake lock")?;

    // Ignore the root node which is unlocked. This happens in cycle flake inputs.
    // TODO: Should come up a way to correctly handle it in database.
    inputs.retain(|&(_, node)| !std::ptr::eq(node, root_node));

    let hashes = inputs
        .iter()
        .map(|(input_name, node)| {
            let hash = &node
                .locked
                .as_ref()
                .with_context(|| format!("Input {input_name:?} is not locked"))?
                .nar_hash;

            // Validate since we'll wrap this in Nix strings below.
            ensure!(
                hash.bytes().all(|b| b != b'\\' && b != b'"'),
                "Invalid nar hash",
            );
            Ok(format!("\"{hash}\" "))
        })
        .collect::<Result<String>>()?;

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
    )
    .await?;

    let resolved = std::iter::zip(inputs, store_paths)
        .map(|((input_name, node), store_path)| {
            let resolved = ResolvedInput {
                is_flake: node.flake,
                store_path,
            };
            (input_name.to_owned(), resolved)
        })
        .collect();
    Ok(resolved)
}

#[derive(Debug)]
struct Resolver<'a> {
    lock: &'a FlakeLock,
    // Prevent infinite recursion when `follows` form a cycle.
    visiting: HashSet<(&'a str, &'a str)>,
}

impl<'a> Resolver<'a> {
    fn new(lock: &'a FlakeLock) -> Self {
        Self {
            lock,
            visiting: HashSet::with_capacity(lock.nodes.len()),
        }
    }

    fn get_node(&mut self, node_id: &str) -> Result<&'a FlakeNode> {
        self.lock
            .nodes
            .get(node_id)
            .with_context(|| format!("Missing node {node_id:?}"))
    }

    fn resolve_input_node_id(&mut self, input: &'a FlakeInput) -> Result<&'a str> {
        let path = match input {
            FlakeInput::Node(node_id) => return Ok(node_id),
            FlakeInput::Follow(path) => path,
        };
        path.iter()
            .try_fold(&*self.lock.root, |node_id, input_name| {
                let input = self
                    .get_node(node_id)?
                    .inputs
                    .get(input_name)
                    .with_context(|| {
                        format!("Missing input {input_name:?} for node {node_id:?}")
                    })?;
                ensure!(
                    self.visiting.insert((node_id, input_name)),
                    "Cyclic 'follows'"
                );
                let ret = self.resolve_input_node_id(input)?;
                self.visiting.remove(&(node_id, input_name));
                Ok(ret)
            })
    }

    fn resolve_node_inputs(&mut self, node_id: &str) -> Result<Vec<(&'a str, &'a FlakeNode)>> {
        self.get_node(node_id)?
            .inputs
            .iter()
            .map(|(input_name, input)| {
                let target_id = self.resolve_input_node_id(input)?;
                let resolved = self.get_node(target_id)?;
                Ok((&**input_name, resolved))
            })
            .collect()
    }
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

// NB. The output of `nix flake archive` doesn't contain followed inputs. We should still use
// call `resolve_flake_locked_inputs` for all resolved inputs.
pub async fn archive(nix_command: &Path) -> Result<()> {
    let output = Command::new(nix_command)
        .kill_on_drop(true)
        .args([
            "flake",
            "archive",
            "--experimental-features",
            "nix-command flakes",
            "--json",
        ])
        .stdin(Stdio::null())
        // Configures stdout/stderr automatically.
        .output()
        .await
        .context("Failed to spawn `nix`")?;

    ensure!(
        output.status.success(),
        "`nix flake archive` failed with {}. Stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr),
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore = "requires calling 'nix'"]
    async fn resolve_flake_lock_inputs() {
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
        let got = resolve_flake_locked_inputs("nix".as_ref(), lock_src)
            .await
            .unwrap();
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

    #[tokio::test]
    #[ignore = "requires calling 'nix'"]
    async fn archive() {
        super::archive("nix".as_ref()).await.unwrap();
    }
}
