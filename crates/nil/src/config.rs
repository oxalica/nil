use anyhow::ensure;
use lsp_types::Url;
use std::collections::HashSet;
use std::path::PathBuf;

pub const CONFIG_KEY: &str = "nil";

macro_rules! define_config {
    (
        $(#[$meta:meta])*
        $vis:vis struct $config:ident {
            $(
            $(#[parse($pointer:literal $(, default = $default:expr)? $(, parse = $parse:path)? )])?
            $field_vis:vis $field:ident : $field_ty:ty,
            )*
        }
    ) => {
        $(#[$meta])*
        $vis struct $config {
            $(
            $field_vis $field : $field_ty,
            )*
        }

        impl $config {
            pub fn new(root_path: PathBuf) -> Self {
                assert!(root_path.is_absolute());
                Self {
                    root_path,
                    $($(
                    $field : define_config!(@default $($default)?),
                    )?)*
                }
            }

            #[allow(clippy::redundant_closure_call)]
            pub fn update(&mut self, mut v: serde_json::Value, errors: &mut Vec<String>) {
                $($(
                if let Some(v) = v.pointer_mut($pointer) {
                    match serde_json::from_value(v.take())
                        .map_err(anyhow::Error::from)
                        $(.and_then(|v| $parse(self, v)))?
                    {
                        Ok(v) => self.$field = v,
                        Err(err) => errors.push(format!(
                            "invalid value of `{}`: {}",
                            $pointer[1..].replace('/', "."),
                            err,
                        )),
                    }
                }
                )?)*
            }
        }
    };
    (@default) => {
        Default::default()
    };
    (@default $expr:expr) => {
        $expr
    };
}

#[macro_rules_attribute::apply(define_config!)]
#[derive(Debug, Clone)]
pub struct Config {
    pub root_path: PathBuf,

    #[parse("/diagnostics/excludedFiles", parse = Config::parse_rooted_file_paths)]
    pub diagnostics_excluded_files: Vec<Url>,
    #[parse("/diagnostics/ignored")]
    pub diagnostics_ignored: HashSet<String>,
    #[parse("/formatting/command", parse = Config::parse_optional_command)]
    pub formatting_command: Option<Vec<String>>,
    #[parse("/nix/binary", default = "nix".into())]
    pub nix_binary: PathBuf,
    #[parse("/nix/maxMemoryMB", default = Some(2048))]
    pub nix_max_memory_mb: Option<u64>,
    #[parse("/nix/flake/autoArchive")]
    pub nix_flake_auto_archive: Option<bool>,
    #[parse("/nix/flake/autoEvalInputs")]
    pub nix_flake_auto_eval_inputs: bool,
    #[parse("/nix/flake/nixpkgsInputName", default = Some("nixpkgs".into()))]
    pub nix_flake_nixpkgs_input_name: Option<String>,
}

impl Config {
    fn parse_rooted_file_paths(&mut self, v: Vec<String>) -> anyhow::Result<Vec<Url>> {
        Ok(v.into_iter()
            .map(|path| {
                Url::from_file_path(self.root_path.join(path)).expect("Root path is absolute")
            })
            .collect())
    }

    fn parse_optional_command(
        &mut self,
        v: Option<Vec<String>>,
    ) -> anyhow::Result<Option<Vec<String>>> {
        ensure!(v != Some(Vec::new()), "command must not be empty");
        Ok(v)
    }

    pub fn nix_max_memory(&self) -> Option<u64> {
        self.nix_max_memory_mb?.checked_mul(1 << 20)
    }
}
