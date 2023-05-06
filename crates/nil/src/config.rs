use lsp_types::Url;
use std::collections::HashSet;
use std::path::PathBuf;

pub const CONFIG_KEY: &str = "nil";

#[derive(Debug, Clone)]
pub struct Config {
    pub root_path: PathBuf,

    pub diagnostics_excluded_files: Vec<Url>,
    pub diagnostics_ignored: HashSet<String>,
    pub formatting_command: Option<Vec<String>>,
    pub nix_binary: PathBuf,
    pub nix_flake_auto_archive: Option<bool>,
}

impl Config {
    pub fn new(root_path: PathBuf) -> Self {
        assert!(root_path.is_absolute());
        Self {
            root_path,
            diagnostics_excluded_files: Vec::new(),
            diagnostics_ignored: HashSet::new(),
            formatting_command: None,
            nix_binary: "nix".into(),
            nix_flake_auto_archive: None,
        }
    }

    // TODO: Simplify.
    pub fn update(&mut self, mut value: serde_json::Value) -> (Vec<String>, bool) {
        let mut errors = Vec::new();
        let mut updated_diagnostics = false;

        if let Some(v) = value.pointer_mut("/diagnostics/excludedFiles") {
            match serde_json::from_value::<Vec<String>>(v.take()) {
                Ok(v) => {
                    self.diagnostics_excluded_files = v
                        .into_iter()
                        .map(|path| {
                            Url::from_file_path(self.root_path.join(path))
                                .expect("Root path is absolute")
                        })
                        .collect();
                    updated_diagnostics = true;
                }
                Err(e) => {
                    errors.push(format!("Invalid value of `diagnostics.excludedFiles`: {e}"));
                }
            }
        }
        if let Some(v) = value.pointer_mut("/diagnostics/ignored") {
            match serde_json::from_value(v.take()) {
                Ok(v) => {
                    self.diagnostics_ignored = v;
                    updated_diagnostics = true;
                }
                Err(e) => {
                    errors.push(format!("Invalid value of `diagnostics.ignored`: {e}"));
                }
            }
        }
        if let Some(v) = value.pointer_mut("/formatting/command") {
            match serde_json::from_value::<Option<Vec<String>>>(v.take()) {
                Ok(Some(v)) if v.is_empty() => {
                    errors.push("`formatting.command` must not be an empty list".into());
                }
                Ok(v) => {
                    self.formatting_command = v;
                }
                Err(e) => {
                    errors.push(format!("Invalid value of `formatting.command`: {e}"));
                }
            }
        }

        if let Some(v) = value.pointer_mut("/nix/binary") {
            match serde_json::from_value::<PathBuf>(v.take()) {
                Ok(path) => {
                    self.nix_binary = path;
                }
                Err(e) => {
                    errors.push(format!("Invalid value of `nix.binary`: {e}"));
                }
            }
        }

        if let Some(v) = value.pointer_mut("/nix/flake/autoArchive") {
            match serde_json::from_value::<Option<bool>>(v.take()) {
                Ok(value) => {
                    self.nix_flake_auto_archive = value;
                }
                Err(e) => {
                    errors.push(format!("Invalid value of `nix.flake.autoArchive`: {e}"));
                }
            }
        }

        (errors, updated_diagnostics)
    }
}
