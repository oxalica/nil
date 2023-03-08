use std::fmt;

use syntax::semantic::escape_literal_attr;

use super::Ty;

#[derive(Debug, Clone, Copy)]
pub struct Config {
    pub max_lambda_lhs_depth: usize,
    pub max_list_depth: usize,
    pub max_attrset_depth: usize,
    pub max_attrset_fields: usize,
    pub lambda_need_parentheses: bool,
}

impl Config {
    pub const FULL: Self = Self {
        max_lambda_lhs_depth: usize::MAX,
        max_list_depth: usize::MAX,
        max_attrset_depth: usize::MAX,
        max_attrset_fields: usize::MAX,
        lambda_need_parentheses: false,
    };
}

#[derive(Clone)]
pub struct TyDisplay<'a> {
    ty: &'a Ty,
    config: Config,
}

impl<'a> TyDisplay<'a> {
    pub fn new(ty: &'a Ty, config: Config) -> Self {
        Self { ty, config }
    }
}

impl fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut config = self.config;
        match self.ty {
            Ty::Unknown => "?".fmt(f),
            Ty::Bool => "bool".fmt(f),
            Ty::Int => "int".fmt(f),
            Ty::Float => "float".fmt(f),
            Ty::String => "string".fmt(f),
            Ty::Path => "path".fmt(f),
            Ty::List(ty) => {
                if config.max_list_depth == 0 {
                    return "[…]".fmt(f);
                }
                config.max_list_depth -= 1;
                config.lambda_need_parentheses = false;
                let elem = Self { ty, config };
                write!(f, "[{elem}]")
            }
            Ty::Lambda(lhs, rhs) => {
                if config.max_lambda_lhs_depth == 0 {
                    return "lambda".fmt(f);
                }

                if config.lambda_need_parentheses {
                    "(".fmt(f)?;
                }
                let mut lhs = Self { ty: lhs, config };
                lhs.config.lambda_need_parentheses = true;
                lhs.config.max_lambda_lhs_depth -= 1;
                // No limit on RHS, so that all arguments are displayed.
                let mut rhs = Self { ty: rhs, config };
                rhs.config.lambda_need_parentheses = false;
                write!(f, "{lhs} → {rhs}")?;
                if config.lambda_need_parentheses {
                    ")".fmt(f)?;
                }
                Ok(())
            }
            Ty::Attrset(set) => {
                if config.max_attrset_depth == 0 {
                    return "{…}".fmt(f);
                }
                config.max_attrset_depth -= 1;
                config.lambda_need_parentheses = false;

                "{".fmt(f)?;
                let mut first = true;
                for (name, ty, _src) in set.iter().take(config.max_attrset_fields) {
                    if first {
                        first = false;
                    } else {
                        ",".fmt(f)?;
                    }
                    let name = escape_literal_attr(name);
                    let value = Self { ty, config };
                    write!(f, " {name}: {value}")?;
                }
                match (config.max_attrset_fields.checked_sub(set.len()), &set.rest) {
                    (Some(1..), Some(rest)) => {
                        if !set.is_empty() {
                            ",".fmt(f)?;
                        }
                        let value = Self {
                            ty: &rest.0,
                            config,
                        };
                        write!(f, " _: {value} }}")
                    }
                    (Some(_), _) => " }".fmt(f),
                    (None, _) => ", … }".fmt(f),
                }
            }
        }
    }
}
