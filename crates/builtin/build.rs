use serde::Deserialize;
use std::path::Path;
use std::process::{Command, Stdio};
use std::{env, fs};

fn main() {
    // Disable rebuild when source files changed.
    println!("cargo:rerun-if-changed=build.rs");

    let builtin_names: Vec<String> = Command::new("nix-instantiate")
        .args(["--eval", "--json", "--expr", "builtins.attrNames builtins"])
        .json()
        .expect("Failed to get builtins");

    // Probe each builtin names to filter all global names. Prim-ops are not included.
    // Here we run them in parallel. There are hundreds of names to test.
    #[allow(clippy::needless_collect)]
    let global_names: Vec<bool> = {
        builtin_names
            .iter()
            .map(|name| {
                Command::new("nix-instantiate")
                    .args(["--parse", "--expr", name])
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

    let mut phf_gen = phf_codegen::Map::<&'static str>::new();
    for (name, is_global) in builtin_names.iter().zip(&global_names) {
        let name = &**name;
        let kind = match name {
            "builtins" => "Attrset",
            "true" | "false" | "null" => "Const",
            _ => "Function",
        };
        phf_gen.entry(
            name,
            &format!(
                "crate::Builtin {{ kind: crate::BuiltinKind::{kind}, is_global: {is_global} }}"
            ),
        );
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
