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
                if config.max_attrset_depth == 0 || config.max_attrset_fields == 0 {
                    return "{…}".fmt(f);
                }
                config.max_attrset_depth -= 1;
                config.lambda_need_parentheses = false;

                "{".fmt(f)?;
                let mut first = true;
                let max_fields = config
                    .max_attrset_fields
                    .saturating_sub(set.rest.is_some() as usize);
                for (name, ty, _src) in set.iter().take(max_fields) {
                    if first {
                        first = false;
                    } else {
                        ",".fmt(f)?;
                    }
                    let name = escape_literal_attr(name);
                    let value = Self { ty, config };
                    write!(f, " {name}: {value}")?;
                }
                if let Some(rest) = &set.rest {
                    if !first {
                        ",".fmt(f)?;
                    }
                    let value = Self {
                        ty: &rest.0,
                        config,
                    };
                    write!(f, " …: {value} }}")
                } else if set.len() > max_fields {
                    ", … }".fmt(f)
                } else {
                    " }".fmt(f)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use super::{Config, TyDisplay};
    use crate::ty::Ty;

    #[track_caller]
    fn check_max_fields(max_attrset_fields: usize, ty: &Ty, expect: Expect) {
        let disp = TyDisplay {
            ty,
            config: Config {
                max_attrset_fields,
                ..super::Config::FULL
            },
        };
        expect.assert_eq(&disp.to_string());
    }

    #[test]
    fn attrset() {
        let ty = &ty!({ "a": int, "b": string, "c": bool });
        check_max_fields(0, ty, expect!["{…}"]);
        check_max_fields(1, ty, expect!["{ a: int, … }"]);
        check_max_fields(2, ty, expect!["{ a: int, b: string, … }"]);
        check_max_fields(3, ty, expect!["{ a: int, b: string, c: bool }"]);
        check_max_fields(4, ty, expect!["{ a: int, b: string, c: bool }"]);
    }

    #[test]
    fn attrset_rest() {
        let ty = &ty!({ "a": int, "b": string, _: bool });
        check_max_fields(0, ty, expect!["{…}"]);
        check_max_fields(1, ty, expect!["{ …: bool }"]);
        check_max_fields(2, ty, expect!["{ a: int, …: bool }"]);
        check_max_fields(3, ty, expect!["{ a: int, b: string, …: bool }"]);
        check_max_fields(4, ty, expect!["{ a: int, b: string, …: bool }"]);
    }
}
