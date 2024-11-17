use std::collections::HashMap;
use std::sync::Arc;

use if_chain::if_chain;
use smol_str::SmolStr;

use crate::{DefDatabase, FileId, Module};

use super::{BindingValue, Expr, ExprId, NameId};

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
        outputs_expr: Option<ExprId>,
    },
    /// A package definition as the first argument of `callPackage`.
    Package {
        /// The lambda expression accepting package dependencies.
        lambda_expr: ExprId,
    },
    /// A NixOS module definition as an `Lambda` of `Attrset` with special fields like `options`.
    ConfigModule {
        /// The lambda expression accepting specialArgs.
        lambda_expr: ExprId,
    },
    /// A NixOS configuration as an `Lambda` of `Attrset`.
    Config {
        /// The lambda expression accepting specialArgs.
        lambda_expr: ExprId,
    },
}

impl ModuleKind {
    /// Get the [ModuleKind] of a [FileId].
    /// Either look it up from the DB or [guess] it.
    pub(crate) fn module_kind_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleKind> {
        let module = db.module(file_id);

        // Check if it is the flake definition. This is always accurate.
        if let Some(flake_info) = db.source_root_flake_info(db.file_source_root(file_id)) {
            if flake_info.flake_file == file_id {
                return Arc::new(parse_flake_nix(&module));
            }
        }

        Arc::new(guess(&module))
    }
}

/// Parse a nix flake.
/// Extract inputs and outputs of the flake.
fn parse_flake_nix(module: &Module) -> ModuleKind {
    let mut explicit_inputs = HashMap::new();
    let mut param_inputs = HashMap::new();
    let mut outputs_expr = None;
    if let Expr::Attrset(flake_set) | Expr::RecAttrset(flake_set) = &module[module.entry_expr()] {
        for &(name_id, value) in flake_set.statics.iter() {
            let BindingValue::Expr(value_expr) = value else {
                continue;
            };
            match &*module[name_id].text {
                "inputs" => {
                    let (Expr::Attrset(inputs) | Expr::RecAttrset(inputs)) = &module[value_expr]
                    else {
                        continue;
                    };
                    explicit_inputs = inputs
                        .statics
                        .iter()
                        .map(|&(input_name_id, _)| {
                            (module[input_name_id].text.clone(), input_name_id)
                        })
                        .collect();
                }
                "outputs" => {
                    outputs_expr = Some(value_expr);
                    let Expr::Lambda(_, Some(pat), _) = &module[value_expr] else {
                        continue;
                    };
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
        outputs_expr,
    }
}

/// Guess the type of a module.
/// Returns either [ModuleKind::Package] or [ModuleKind::Config].
fn guess(module: &Module) -> ModuleKind {
    let entry_expr = peel_expr(module, module.entry_expr);

    // Try to parse as package definition.
    if_chain! {        // Must be a lambda expression with Pat.
        if let Expr::Lambda(_, Some(_pat), body_expr) = &module[entry_expr];
        // The body must be a reference or function application (typically,
        // `stdenv.mkDerivation`).
        let body_expr = peel_expr(module, *body_expr);
        if matches!(module[body_expr], Expr::Apply(..));
        then {
            return ModuleKind::Package { lambda_expr: entry_expr };
        }
    }

    // Try to parse as NixOS module or config with top-level Lambda.
    if_chain! {
        // Must be a lambda expression with Pat.
        if let Expr::Lambda(_, Some(pat), body_expr) = &module[entry_expr];
        // Pat must have ellipsis.
        if pat.ellipsis;
        // The body must be an attrset.
        let body_expr = peel_expr(module, *body_expr);
        if let Expr::Attrset(bindings) | Expr::RecAttrset(bindings) = &module[body_expr];
        then {
            // If it has special fields, it is a NixOS module definition.
            if bindings
                .statics
                .iter()
                .any(|&(name, _)| matches!(&*module[name].text, "options" | "config" | "meta")) {
                return ModuleKind::ConfigModule { lambda_expr: entry_expr };
            }
            return ModuleKind::Config { lambda_expr: entry_expr };
        }
    }

    ModuleKind::Unknown
}

/// Peel all environment-like wrapper expression like `With`, `Assert` and `LetIn`.
fn peel_expr(module: &Module, expr: ExprId) -> ExprId {
    std::iter::successors(Some(expr), |&e| match &module[e] {
        Expr::With(_, inner) | Expr::Assert(_, inner) | Expr::LetIn(_, inner) => Some(*inner),
        _ => None,
    })
    .last()
    .unwrap()
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use itertools::Itertools;

    use super::*;
    use crate::tests::TestDB;
    use crate::SourceDatabase;

    #[track_caller]
    fn check(src: &str, expect: Expect) {
        let (db, file) = TestDB::single_file(src).unwrap();
        let src = db.file_content(file);
        let source_map = db.source_map(file);
        let expr_header = |expr| {
            let lambda = &src[source_map.node_for_expr(expr).unwrap().text_range()];
            lambda.lines().next().unwrap().to_owned()
        };
        let got = match &*db.module_kind(file) {
            ModuleKind::Unknown => "Unknown".to_owned(),
            ModuleKind::FlakeNix {
                explicit_inputs,
                param_inputs,
                outputs_expr,
            } => {
                let explicit_inputs = explicit_inputs.keys().sorted().join(",");
                let param_inputs = param_inputs.keys().sorted().join(",");
                let outputs = outputs_expr.map(expr_header).unwrap_or_default();
                format!("FlakeNix: explicit_inputs={explicit_inputs} param_inputs={param_inputs} outputs={outputs}")
            }
            ModuleKind::Package { lambda_expr } => {
                format!("Package: {}", expr_header(*lambda_expr))
            }
            ModuleKind::ConfigModule { lambda_expr } => {
                format!("ConfigModule: {}", expr_header(*lambda_expr))
            }
            ModuleKind::Config { lambda_expr } => {
                format!("Config: {}", expr_header(*lambda_expr))
            }
        };
        expect.assert_eq(&got);
    }

    #[test]
    fn flake_nix_normal() {
        check(
            r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee
{
    inputs.nil.url = "github:oxalica/nil";
    outputs = { self, nixpkgs, ... }: { };
}
            "#,
            expect!["FlakeNix: explicit_inputs=nil param_inputs=nixpkgs outputs={ self, nixpkgs, ... }: { }"],
        );
    }

    #[test]
    fn flake_nix_rec() {
        check(
            r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee
rec {
    inputs = rec {
        nil = rec {
            url = "github:oxalica/nil";
        };
    };
    outputs = { self, nixpkgs, ... }: rec { };
}
            "#,
            expect!["FlakeNix: explicit_inputs=nil param_inputs=nixpkgs outputs={ self, nixpkgs, ... }: rec { }"],
        );
    }

    #[test]
    fn package() {
        check(
            "
{ stdenv, foo, bar }:
let path = [ ]; in
stdenv.mkDerivation { }
            ",
            expect!["Package: { stdenv, foo, bar }:"],
        );
    }

    #[test]
    fn module() {
        check(
            "
with builtins;
{ lib, config, ... }:
with lib;
let cfg = config.foo; in
{
    options = { };
    config = lib.mkIf true { };
}
            ",
            expect!["ConfigModule: { lib, config, ... }:"],
        );
    }

    #[test]
    fn config() {
        check(
            "
{ lib, pkgs, ... }:
with lib;
{
    environment.systemPackages = with pkgs; [ hello ];
}
            ",
            expect!["Config: { lib, pkgs, ... }:"],
        );
    }
}
