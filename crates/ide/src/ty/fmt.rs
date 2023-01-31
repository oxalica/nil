use std::fmt;

use super::Ty;

const MAX_FIELD_CNT: usize = 8;

#[derive(Clone)]
pub struct TyDisplay<'a> {
    ty: &'a Ty,
    depth: usize,
    in_param: bool,
}

impl<'a> TyDisplay<'a> {
    pub fn new(ty: &'a Ty, depth: usize) -> Self {
        Self {
            ty,
            depth,
            in_param: false,
        }
    }
}

impl fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Ty::Unknown => "?".fmt(f),
            Ty::Bool => "bool".fmt(f),
            Ty::Int => "int".fmt(f),
            Ty::Float => "float".fmt(f),
            Ty::String => "string".fmt(f),
            Ty::Path => "path".fmt(f),
            Ty::List(ty) => {
                if self.depth == 0 {
                    "[…]".fmt(f)
                } else {
                    let elem = Self {
                        ty,
                        depth: self.depth - 1,
                        in_param: false,
                    };
                    write!(f, "[{}]", elem)
                }
            }
            Ty::Lambda(param, ret) => {
                if self.in_param {
                    "(".fmt(f)?;
                }
                if self.depth == 0 {
                    "… → …".fmt(f)?;
                } else {
                    let param = Self {
                        ty: param,
                        // Show full lambda type.
                        depth: self.depth,
                        in_param: true,
                    };
                    let ret = Self {
                        ty: ret,
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
            Ty::Attrset(set) => {
                if self.depth == 0 {
                    "{ … }".fmt(f)
                } else {
                    "{".fmt(f)?;
                    let mut first = true;
                    for (name, ty, _src) in set.iter().take(MAX_FIELD_CNT) {
                        if first {
                            first = false;
                        } else {
                            ",".fmt(f)?;
                        }
                        // FIXME: Escape field names.
                        let value = Self {
                            ty,
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
