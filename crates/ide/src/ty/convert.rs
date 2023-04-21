//! Convert structures from Nix evaluation result into `Ty`s.
use std::collections::HashMap;
use std::sync::Arc;

use nix_interop::flake_output::{FlakeOutput, Type as OutputTy};
use nix_interop::nixos_options::Ty as OptionTy;

use crate::{SourceRootId, TyDatabase};

use super::{AttrSource, Attrset, Ty};

pub(crate) fn options_to_config_ty(db: &dyn TyDatabase) -> Ty {
    let opts = db.nixos_options();
    let fields = opts
        .iter()
        .map(|(name, opt)| (name.as_str(), from_raw_ty(&opt.ty), AttrSource::Unknown));
    Ty::Attrset(Attrset::from_internal(fields, None))
}

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

pub(crate) fn flake_input_tys(db: &dyn TyDatabase, sid: SourceRootId) -> Arc<HashMap<String, Ty>> {
    let Some(info) = db.source_root_flake_info(sid) else { return Arc::default() };
    let tys = info
        .input_flake_outputs
        .iter()
        .map(|(name, output)| (name.clone(), from_flake_output(output)))
        .collect();
    Arc::new(tys)
}

fn from_flake_output(out: &FlakeOutput) -> Ty {
    match out {
        FlakeOutput::Leaf(leaf) => match leaf.type_ {
            OutputTy::NixosModule => ty!({}),
            OutputTy::Derivation => ty!(derivation),
            OutputTy::Unknown => ty!(?),
        },
        FlakeOutput::Attrset(set) => {
            let fields = set
                .iter()
                .map(|(key, output)| (&**key, from_flake_output(output), AttrSource::Unknown));
            Ty::Attrset(Attrset::from_internal(fields, None))
        }
    }
}
