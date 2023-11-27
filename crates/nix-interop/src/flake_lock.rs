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
use crate::FlakeUrl;

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
    // Workaround: It's currently impossible to calculate store paths of type="file" inputs
    // given only `flake.lock`.
    // Ref: https://github.com/oxalica/nil/issues/113
    // and https://github.com/NixOS/nix/issues/9456
    inputs.retain(|&(_, node)| {
        !std::ptr::eq(node, root_node) && !matches!(&node.locked, Some(f) if f._type == "file")
    });

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
    #[serde(rename = "type")]
    _type: String,
    // ...
}

// NB. The output of `nix flake archive` doesn't contain followed inputs. We should still use
// call `resolve_flake_locked_inputs` for all resolved inputs.
pub async fn archive(nix_command: &Path, flake_url: &FlakeUrl) -> Result<()> {
    let output = Command::new(nix_command)
        .kill_on_drop(true)
        .args([
            "flake",
            "archive",
            "--experimental-features",
            "nix-command flakes",
            "--json",
        ])
        .arg(flake_url)
        .stdin(Stdio::null())
        // Configures stdout/stderr automatically.
        .output()
        .await
        .context("Failed to spawn `nix`")?;

    ensure!(
        output.status.success(),
        "`nix flake archive {}` failed with {}. Stderr:\n{}",
        flake_url,
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
        let lock_src = std::fs::read("./tests/test_flake/flake.lock").unwrap();
        let got = resolve_flake_locked_inputs("nix".as_ref(), &lock_src)
            .await
            .unwrap();
        // NB. This does not contain `non-flake-file`,
        // see the comment in `resolve_flake_locked_inputs`.
        let expect = HashMap::from_iter([
            (
                "nixpkgs".to_owned(),
                ResolvedInput {
                    store_path: "/nix/store/hap5a6iw5rccl21adfxh5b3lk2c8qnmj-source".to_owned(),
                    is_flake: true,
                },
            ),
            (
                "nix".to_owned(),
                ResolvedInput {
                    store_path: "/nix/store/5598lqiaw5qjgn661w74q2a6kivgiksa-source".to_owned(),
                    is_flake: false,
                },
            ),
        ]);
        assert_eq!(got, expect);
    }

    #[tokio::test]
    #[ignore = "requires calling 'nix' and network access"]
    async fn archive() {
        let flake = FlakeUrl::new_path("./tests/test_flake");
        super::archive("nix".as_ref(), &flake).await.unwrap();
    }
}
