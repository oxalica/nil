use std::collections::HashMap;
use std::path::Path;
use std::process::Stdio;

use anyhow::{ensure, Context, Result};
use serde::Deserialize;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;
use tokio::sync::watch;

use crate::{FlakeUrl, NixOutOfMemory};

pub async fn eval_flake_output(
    nix_command: &Path,
    flake_url: &FlakeUrl,
    watcher_tx: Option<watch::Sender<String>>,
    legacy: bool,
    memory_limit: Option<u64>,
) -> Result<FlakeOutput> {
    let mut command = Command::new(nix_command);
    command
        .kill_on_drop(true)
        .args([
            "flake",
            "show",
            "--experimental-features",
            "nix-command flakes",
            "--json",
        ])
        .args(watcher_tx.is_some().then_some("-v"))
        .args(legacy.then_some("--legacy"))
        .arg(flake_url)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    #[cfg(unix)]
    unsafe {
        if let Some(limit) = memory_limit {
            use rustix::process::{setrlimit, Resource, Rlimit};
            command.pre_exec(move || {
                // NB. RSS limit has no effect on modern Linux. We set DATA limit instead.
                setrlimit(
                    Resource::Data,
                    Rlimit {
                        current: Some(limit),
                        maximum: Some(limit),
                    },
                )
                .map_err(|err| std::io::Error::from_raw_os_error(err.raw_os_error()))
            });
        }
    }

    #[cfg(not(unix))]
    let _unused = memory_limit;

    let mut child = command.spawn().context("Failed to spawn `nix`")?;

    let stderr = child.stderr.take().expect("Piped");
    let mut error_msg = String::new();
    let mut oom = false;
    let consume_stderr_fut = async {
        let mut stderr = BufReader::new(stderr);
        let mut line = String::new();
        while {
            line.clear();
            matches!(stderr.read_line(&mut line).await, Ok(n) if n != 0)
        } {
            let line = line.trim();
            if let Some(inner) = line.strip_prefix("evaluating '") {
                if let Some(inner) = inner.strip_suffix("'...") {
                    if let Some(tx) = &watcher_tx {
                        tx.send_modify(|buf| {
                            buf.clear();
                            buf.push_str(inner);
                        });
                    }
                }
            } else {
                if line == "error: out of memory" {
                    oom = true;
                }
                error_msg.push_str(line);
            }
        }
    };
    let wait_fut = child.wait_with_output();
    let output = tokio::join!(consume_stderr_fut, wait_fut).1?;

    if oom {
        return Err(NixOutOfMemory.into());
    }

    ensure!(
        output.status.success(),
        "`nix flake show {}` failed with {}. Stderr:\n{}",
        flake_url,
        output.status,
        error_msg,
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
    #[ignore = "requires calling 'nix' and network access"]
    async fn eval_outputs() {
        let flake_url = FlakeUrl::new_path("./tests/test_flake");
        let (tx, rx) = watch::channel(String::new());
        let output = eval_flake_output("nix".as_ref(), &flake_url, Some(tx), false, None)
            .await
            .unwrap();
        // Even if the system is omitted, the attrpath is still printed in progress.
        assert_eq!(*rx.borrow(), "packages.x86_64-linux.hello");

        let system = crate::tests::get_nix_system().await;
        let leaf = (|| {
            output.as_attrset()?["packages"].as_attrset()?[&system].as_attrset()?["hello"].as_leaf()
        })()
        .unwrap();
        assert_eq!(leaf.type_, Type::Derivation);
        assert_eq!(leaf.name.as_ref().unwrap(), "hello-1.2.3");
        assert_eq!(leaf.description.as_deref(), Some("A test derivation"));
    }

    #[tokio::test]
    #[ignore = "requires calling 'nix'"]
    async fn memory_limit() {
        let flake_url = FlakeUrl::new_path("./tests/oom_flake");
        // 64MiB. This should be large enough to start the Nix evaluator itself without crash.
        let limit = 64 << 20;
        let err = eval_flake_output("nix".as_ref(), &flake_url, None, false, Some(limit))
            .await
            .unwrap_err();
        assert!(err.is::<NixOutOfMemory>(), "expect OOM but got: {err}");
    }
}
