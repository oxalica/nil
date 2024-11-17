//! Convert structures from Nix evaluation result into `Ty`s.
use std::collections::HashMap;
use std::sync::Arc;

use nix_interop::flake_output::{FlakeOutput, Type as OutputTy};
use nix_interop::nixos_options::Ty as OptionTy;

use crate::{SourceRootId, TyDatabase};

use super::known::FLAKE_OUTPUT_GENERIC_SYSTEM_FIELDS;
use super::{AttrSource, Attrset, Ty};

// TODO: Get this at runtime.
const NIX_SYSTEM: &str = "x86_64-linux";

pub(crate) fn options_to_config_ty(db: &dyn TyDatabase) -> Ty {
    let opts = db.nixos_options();
    let fields = opts
        .iter()
        .map(|(name, opt)| (name.as_str(), from_raw_ty(&opt.ty), AttrSource::Unknown));
    Ty::Attrset(Attrset::from_internal(fields, None))
}

/// Create a [Ty] from [OptionTy].
/// [OptionTy] was automically retrieved from nixpks option declaration.
fn from_raw_ty(ty: &OptionTy) -> Ty {
    match ty {
        OptionTy::Any => ty!(?),
        OptionTy::Bool => ty!(bool),
        OptionTy::Int => ty!(int),
        OptionTy::Float => ty!(float),
        OptionTy::String => ty!(string),
        OptionTy::Path => ty!(path),
        OptionTy::Derivation => ty!(derivation),
        OptionTy::List { elem } => ty!([(#from_raw_ty(elem))]),
        OptionTy::Lambda { from, to } => ty!((#from_raw_ty(from)) -> (#from_raw_ty(to))),
        OptionTy::Attrset { fields, rest } => {
            let fields = fields
                .iter()
                .map(|(name, opt)| (name.as_str(), from_raw_ty(&opt.ty), AttrSource::Unknown));
            let rest = rest
                .as_deref()
                .map(|raw_ty| (from_raw_ty(raw_ty), AttrSource::Unknown));
            Ty::Attrset(Attrset::from_internal(fields, rest))
        }
    }
}

/// Get the input types of all flake inputs.
pub(crate) fn flake_input_tys(db: &dyn TyDatabase, sid: SourceRootId) -> Arc<HashMap<String, Ty>> {
    let Some(info) = db.source_root_flake_info(sid) else {
        return Arc::default();
    };
    let tys = info
        .input_flake_outputs
        .iter()
        .map(|(name, output)| (name.clone(), from_flake_output(output)))
        .collect();
    Arc::new(tys)
}

/// Get the type from flake outputs.
fn from_flake_output(out: &FlakeOutput) -> Ty {
    let FlakeOutput::Attrset(set) = out else {
        return from_flake_output_inner(out, None);
    };
    let fields = set.iter().map(|(key, output)| {
        let generic_system_depth = FLAKE_OUTPUT_GENERIC_SYSTEM_FIELDS
            .iter()
            .find_map(|&(k, depth)| (k == key).then_some(depth));
        (
            &**key,
            from_flake_output_inner(output, generic_system_depth),
            AttrSource::Unknown,
        )
    });
    Ty::Attrset(Attrset::from_internal(fields, None))
}

/// Subroutine in [from_flake_output].
fn from_flake_output_inner(out: &FlakeOutput, generic_system_depth: Option<usize>) -> Ty {
    match out {
        FlakeOutput::Leaf(leaf) => match leaf.type_ {
            OutputTy::NixosModule => ty!({}),
            OutputTy::Derivation => ty!(derivation),
            OutputTy::Unknown => ty!(?),
        },
        FlakeOutput::Attrset(set) => {
            let set_rest = generic_system_depth == Some(0);
            let generic_system_depth = generic_system_depth.and_then(|i| i.checked_sub(1));
            let fields = set.iter().map(|(key, output)| {
                (
                    &**key,
                    from_flake_output_inner(output, generic_system_depth),
                    AttrSource::Unknown,
                )
            });
            let mut set = Attrset::from_internal(fields, None);
            if set_rest {
                if let Some(ty) = set.get(NIX_SYSTEM) {
                    set.rest = Some(Arc::new((ty.clone(), AttrSource::Unknown)));
                }
            }
            Ty::Attrset(set)
        }
    }
}
