use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;
use std::process::{Command, Stdio};
use std::{env, fs};

fn main() {
    // Disable rebuild when source files changed.
    println!("cargo:rerun-if-changed=build.rs");

    let builtins_attr_names: Vec<String> = Command::new("nix")
        .args([
            "eval",
            "--experimental-features",
            "nix-command flakes",
            "--store",
            "dummy://",
            "--impure",
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
        builtins_attr_names
            .iter()
            .map(|name| {
                Command::new("nix")
                    .args([
                        "eval",
                        "--experimental-features",
                        "nix-command flakes",
                        "--store",
                        "dummy://",
                        "--impure",
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

    let mut builtin_infos = dump_builtin_infos();
    builtin_infos.sort_by(|lhs, rhs| lhs.name.cmp(&rhs.name));
    assert_eq!(
        builtin_infos.windows(2).find(|w| w[0].name == w[1].name),
        None,
        "no duplicated builtins",
    );

    let mut phf_gen = phf_codegen::Map::<&'static str>::new();
    for (name, is_global) in builtins_attr_names.iter().zip(&global_names) {
        let name = &**name;
        let BuiltinInfo {
            name: _,
            kind,
            doc,
            args,
            impure_only,
            experimental_feature,
        } = match builtin_infos.binary_search_by(|probe| (*probe.name).cmp(name)) {
            Ok(i) => builtin_infos[i].clone(),
            Err(_) => BuiltinInfo {
                name: String::new(), // Not used.
                kind: guess_name_kind(name).into(),
                doc: "(No documentation from Nix)".into(),
                args: Vec::new(),
                impure_only: false,
                experimental_feature: None,
            },
        };

        let summary = ["`builtins.", name]
            .into_iter()
            .chain(args.iter().flat_map(|arg| [" ", arg]))
            .chain(Some("`"))
            .collect::<String>();
        let rhs = format!(
            "crate::Builtin {{
                kind: crate::BuiltinKind::{kind},
                is_global: {is_global},
                summary: {summary:?},
                doc: Some({doc:?}),
                impure_only: {impure_only},
                experimental_feature: {experimental_feature:?},
            }}"
        );
        phf_gen.entry(name, rhs);
    }

    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("generated.expr");
    fs::write(path, phf_gen.build().to_string()).unwrap();
}

fn dump_builtin_infos() -> Vec<BuiltinInfo> {
    // Use a secret subcommand `__dump-language` to dump Nix builtins with documentations.
    // It is introduced since Nix 2.17 in
    // https://github.com/NixOS/nix/commit/22b278e011ab9c1328749a126514c57b89a39173#diff-20a8b5b2a231db80eab27840bd32ac0214aa0c4e9e923e649d3d741c3da77b48L355
    if let Ok(lang) = Command::new("nix")
        .arg("__dump-language")
        .json::<DumpLanguage>()
    {
        let builtins = match lang {
            DumpLanguage::V2 { builtins } => builtins,
            DumpLanguage::V1 {
                mut builtins,
                constants,
            } => {
                builtins.extend(constants);
                builtins
            }
        };
        return builtins
            .into_iter()
            .map(|(name, builtin)| BuiltinInfo {
                name,
                kind: if !builtin.args.is_empty() {
                    "Function"
                } else if builtin.type_.is_some_and(|s| s.eq_ignore_ascii_case("set")) {
                    "Attrset"
                } else {
                    "Const"
                }
                .into(),
                doc: builtin.doc,
                args: builtin.args,
                impure_only: builtin.impure_only,
                experimental_feature: builtin.experimental_feature,
            })
            .collect();
    }

    // Fallback to the older command `__dump-builtins` so that the package
    // doesn't fail to build for people using older versions of nix
    // (introduced in 2.4)
    // https://github.com/NixOS/nix/commit/0f314f3c2594e80322c675b70a61dcfda11bf423#diff-20a8b5b2a231db80eab27840bd32ac0214aa0c4e9e923e649d3d741c3da77b48R187
    let builtins = Command::new("nix")
        .arg("__dump-builtins")
        .json::<DumpBuiltins>()
        .expect("failed to dump builtin docs");

    builtins
        .into_iter()
        .map(|(name, builtin)| BuiltinInfo {
            kind: guess_name_kind(&name).into(),
            name,
            doc: builtin.doc,
            args: builtin.args,
            impure_only: false,
            experimental_feature: None,
        })
        .collect()
}

fn guess_name_kind(name: &str) -> &'static str {
    match name {
        "builtins" => "Attrset",
        "true" | "false" | "null" => "Const",
        _ => "Function",
    }
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct BuiltinInfo {
    name: String,
    kind: String,
    doc: String,
    args: Vec<String>,
    impure_only: bool,
    experimental_feature: Option<String>,
}

/// Nix dump language V1 or V2.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum DumpLanguage {
    // Nix < 2.24
    V1 {
        builtins: DumpBuiltins,
        constants: DumpBuiltins,
    },
    // Nix >= 2.24
    V2 {
        #[serde(flatten)]
        builtins: BTreeMap<String, DumpBuiltin>,
    },
}

// Keep names sorted.
type DumpBuiltins = BTreeMap<String, DumpBuiltin>;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct DumpBuiltin {
    #[serde(default)]
    args: Vec<String>,
    doc: String,
    #[serde(default)]
    experimental_feature: Option<String>,
    #[serde(default)]
    impure_only: bool,
    #[serde(rename = "type")]
    type_: Option<String>,
}
