use std::collections::HashMap;
use std::sync::Arc;

use smol_str::SmolStr;

use crate::{DefDatabase, FileId, Module};

use super::{BindingValue, Expr, NameId};

/// Guessed kind of a nix file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleKind {
    /// Uncatagorized or ambiguous.
    Unknown,
    /// Flake definition `flake.nix`.
    FlakeNix {
        /// Explicit inputs defined in top-level `inputs`.
        explicit_inputs: HashMap<SmolStr, NameId>,
        /// Implicit inputs introduced in the pat-parameter of `outputs`.
        /// NB. `self` parameter is special and is excluded here.
        param_inputs: HashMap<SmolStr, NameId>,
    },
}

impl ModuleKind {
    pub(crate) fn module_kind_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleKind> {
        let module = db.module(file_id);

        // Check if it is the flake definition. This is always accurate.
        if let Some(flake_info) = db.source_root_flake_info(db.file_source_root(file_id)) {
            if flake_info.flake_file == file_id {
                return Arc::new(parse_flake_nix(&module));
            }
        }

        Arc::new(ModuleKind::Unknown)
    }
}

fn parse_flake_nix(module: &Module) -> ModuleKind {
    let mut explicit_inputs = HashMap::new();
    let mut param_inputs = HashMap::new();
    if let Expr::Attrset(flake_set) = &module[module.entry_expr()] {
        for &(name_id, value) in flake_set.statics.iter() {
            let BindingValue::Expr(value_expr) = value else { continue };
            match &*module[name_id].text {
                "inputs" => {
                    let Expr::Attrset(inputs) = &module[value_expr] else { continue };
                    explicit_inputs = inputs
                        .statics
                        .iter()
                        .map(|&(input_name_id, _)| {
                            (module[input_name_id].text.clone(), input_name_id)
                        })
                        .collect();
                }
                "outputs" => {
                    let Expr::Lambda(_, Some(pat), _) = &module[value_expr] else { continue };
                    param_inputs = pat
                        .fields
                        .iter()
                        .filter_map(|&(name_id, _)| name_id)
                        .map(|name_id| (module[name_id].text.clone(), name_id))
                        // Exclude `self`.
                        .filter(|(name, _)| name != "self")
                        .collect();
                }
                _ => {}
            }
        }
    }
    ModuleKind::FlakeNix {
        explicit_inputs,
        param_inputs,
    }
}
