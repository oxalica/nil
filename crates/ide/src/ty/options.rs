use nix_interop::nixos_options::Ty as RawTy;

use crate::TyDatabase;

use super::{AttrSource, Attrset, Ty};

pub(crate) fn options_to_config_ty(db: &dyn TyDatabase) -> Ty {
    let opts = db.nixos_options();
    let fields = opts
        .iter()
        .map(|(name, opt)| (name.as_str(), from_raw_ty(&opt.ty), AttrSource::Unknown));
    Ty::Attrset(Attrset::from_internal(fields, None))
}

fn from_raw_ty(raw_ty: &RawTy) -> Ty {
    match raw_ty {
        RawTy::Any => ty!(?),
        RawTy::Bool => ty!(bool),
        RawTy::Int => ty!(int),
        RawTy::Float => ty!(float),
        RawTy::String => ty!(string),
        RawTy::Path => ty!(path),
        RawTy::Derivation => ty!(derivation),
        RawTy::List { elem } => ty!([(#from_raw_ty(elem))]),
        RawTy::Lambda { from, to } => ty!((#from_raw_ty(from)) -> (#from_raw_ty(to))),
        RawTy::Attrset { fields, rest } => {
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
