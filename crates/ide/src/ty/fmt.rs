//! TODO: Limit type length.
use super::{InferenceResult, Ty, TyKind};
use std::fmt;

#[derive(Clone, Copy)]
pub struct TyDisplay<'a> {
    ty: Ty,
    infer: &'a InferenceResult,
}

impl<'a> TyDisplay<'a> {
    pub fn new(ty: Ty, infer: &'a InferenceResult) -> Self {
        Self { ty, infer }
    }

    fn with(self, ty: Ty) -> Self {
        Self::new(ty, self.infer)
    }
}

impl fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.infer.kind(self.ty) {
            TyKind::Unknown => "?".fmt(f),
            TyKind::Bool => "bool".fmt(f),
            TyKind::Int => "int".fmt(f),
            TyKind::Float => "float".fmt(f),
            TyKind::String => "string".fmt(f),
            TyKind::Path => "path".fmt(f),
            &TyKind::List(elem) => write!(f, "[{}]", self.with(elem)),
            &TyKind::Lambda(a, b) => write!(f, "{} -> {}", self.with(a), self.with(b)),
            TyKind::Attrset(set) => {
                write!(f, "{{")?;
                let mut first = true;
                for (name, ty) in set.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ",")?;
                    }
                    // FIXME: Escape field names.
                    write!(f, " {}: {}", name, self.with(ty))?;
                }
                write!(f, " }}")
            }
        }
    }
}
