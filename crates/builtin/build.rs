use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;
use std::process::{Command, Stdio};
use std::{env, fs};

fn main() {
    // Disable rebuild when source files changed.
    println!("cargo:rerun-if-changed=build.rs");

    let builtin_names: Vec<String> = Command::new("nix")
        .args([
            "eval",
            "--experimental-features",
            "nix-command",
            "--store",
            "dummy://",
            "--json",
            "--expr",
            "builtins.attrNames builtins",
        ])
        .json()
        .expect("Failed to get builtins. Is `nix` accessible?");

    // Probe each builtin names to filter all global names. Prim-ops are not included.
    // Here we run them in parallel. There are hundreds of names to test.
    #[allow(clippy::needless_collect)]
    let global_names: Vec<bool> = {
        builtin_names
            .iter()
            .map(|name| {
                Command::new("nix")
                    .args([
                        "eval",
                        "--experimental-features",
                        "nix-command",
                        "--store",
                        "dummy://",
                        "--expr",
                        name,
                    ])
                    .stdin(Stdio::null())
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .spawn()
                    .expect("Failed to spawn")
            })
            // First spawn all.
            .collect::<Vec<_>>()
            .into_iter()
            .map(|mut child| child.wait().expect("Failed to wait").success())
            .collect()
    };

    // Use a secret subcommand `__dump-language` to dump Nix builtins with documentations.
    // It is introduced since Nix 2.17 in
    // https://github.com/NixOS/nix/commit/22b278e011ab9c1328749a126514c57b89a39173#diff-20a8b5b2a231db80eab27840bd32ac0214aa0c4e9e923e649d3d741c3da77b48L355
    let builtins_dump: DumpBuiltins = Command::new("nix")
        .arg("__dump-language")
        .json::<DumpLanguage>()
        .map_or_else(
            |_| {
                // Fallback to the older command `__dump-builtins` so that the package
                // doesn't fail to build for people using older versions of nix
                // (introduced in 2.4)
                // https://github.com/NixOS/nix/commit/0f314f3c2594e80322c675b70a61dcfda11bf423#diff-20a8b5b2a231db80eab27840bd32ac0214aa0c4e9e923e649d3d741c3da77b48R187
                Command::new("nix")
                    .arg("__dump-builtins")
                    .json::<DumpBuiltins>()
            },
            |v| Ok(v.builtins),
        )
        .expect("Failed to dump builtins");

    let mut phf_gen = phf_codegen::Map::<&'static str>::new();
    for (name, is_global) in builtin_names.iter().zip(&global_names) {
        let name = &**name;
        let kind = match name {
            "builtins" => "Attrset",
            "true" | "false" | "null" => "Const",
            _ => "Function",
        };
        let args = builtins_dump
            .get(name)
            .map(|b @ DumpBuiltin { args, arity, .. }| {
                assert_eq!(args.len(), *arity, "Arity mismatch: {b:?}");
                args.iter().flat_map(|arg| [" ", arg]).collect::<String>()
            })
            .unwrap_or_default();
        let summary = format!("`builtins.{name}{args}`");
        let doc = builtins_dump.get(name).map(|b| &b.doc);
        phf_gen.entry(name, &format!("crate::Builtin {{ kind: crate::BuiltinKind::{kind}, is_global: {is_global}, summary: {summary:?}, doc: {doc:?} }}"));
    }

    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("generated.expr");
    fs::write(path, phf_gen.build().to_string()).unwrap();
}

trait CommandExt {
    fn json<T: for<'de> Deserialize<'de>>(&mut self) -> Result<T, Box<dyn std::error::Error>>;
}

impl CommandExt for Command {
    fn json<T: for<'de> Deserialize<'de>>(&mut self) -> Result<T, Box<dyn std::error::Error>> {
        let output = self.output()?;
        if !output.status.success() {
            return Err(format!("Command {self:?} failed: {output:?}").into());
        }
        Ok(serde_json::from_slice(&output.stdout)?)
    }
}

#[derive(Debug, Deserialize)]
struct DumpLanguage {
    builtins: DumpBuiltins,
}

// Keep names sorted.
type DumpBuiltins = BTreeMap<String, DumpBuiltin>;

#[derive(Debug, Deserialize)]
struct DumpBuiltin {
    args: Vec<String>,
    arity: usize,
    doc: String,
}
