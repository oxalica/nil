use super::{InferenceResult, Ty, TyKind};
use std::fmt;

const MAX_FIELD_CNT: usize = 8;

#[derive(Clone, Copy)]
pub struct TyDisplay<'a> {
    ty: Ty,
    infer: &'a InferenceResult,
    depth: usize,
    in_param: bool,
}

impl<'a> TyDisplay<'a> {
    pub fn new(ty: Ty, infer: &'a InferenceResult, depth: usize) -> Self {
        Self {
            ty,
            infer,
            depth,
            in_param: false,
        }
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
            &TyKind::List(ty) => {
                if self.depth == 0 {
                    "[…]".fmt(f)
                } else {
                    let elem = Self {
                        ty,
                        infer: self.infer,
                        depth: self.depth - 1,
                        in_param: false,
                    };
                    write!(f, "[{}]", elem)
                }
            }
            &TyKind::Lambda(param, ret) => {
                if self.in_param {
                    "(".fmt(f)?;
                }
                if self.depth == 0 {
                    "… → …".fmt(f)?;
                } else {
                    let param = Self {
                        ty: param,
                        infer: self.infer,
                        // Show full lambda type.
                        depth: self.depth,
                        in_param: true,
                    };
                    let ret = Self {
                        ty: ret,
                        infer: self.infer,
                        // Show full lambda type.
                        depth: self.depth,
                        in_param: false,
                    };
                    write!(f, "{} → {}", param, ret)?;
                }
                if self.in_param {
                    ")".fmt(f)?;
                }
                Ok(())
            }
            TyKind::Attrset(set) => {
                if self.depth == 0 {
                    "{ … }".fmt(f)
                } else {
                    "{".fmt(f)?;
                    let mut first = true;
                    for (name, ty) in set.iter().take(MAX_FIELD_CNT) {
                        if first {
                            first = false;
                        } else {
                            ",".fmt(f)?;
                        }
                        // FIXME: Escape field names.
                        let value = Self {
                            ty,
                            infer: self.infer,
                            depth: self.depth - 1,
                            in_param: false,
                        };
                        write!(f, " {}: {}", name, value)?;
                    }
                    if set.len() > MAX_FIELD_CNT {
                        ", … }".fmt(f)
                    } else {
                        " }".fmt(f)
                    }
                }
            }
        }
    }
}
