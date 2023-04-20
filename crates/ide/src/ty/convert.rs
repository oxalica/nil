//! Convert structures from Nix evaluation result into `Ty`s.
use nix_interop::nixos_options::Ty as OptionTy;

use crate::TyDatabase;

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
