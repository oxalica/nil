#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Builtin {
    pub kind: BuiltinKind,
    pub is_global: bool,
    pub summary: Option<&'static str>,
    pub doc: Option<&'static str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinKind {
    Const,
    Function,
    Attrset,
}

pub static ALL_BUILTINS: phf::Map<&'static str, Builtin> =
    include!(concat!(env!("OUT_DIR"), "/generated.expr"));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanity() {
        assert_eq!(
            ALL_BUILTINS["true"],
            Builtin {
                kind: BuiltinKind::Const,
                is_global: true,
                summary: None,
                doc: None,
            },
        );

        assert_eq!(
            ALL_BUILTINS["attrNames"],
            Builtin {
                kind: BuiltinKind::Function,
                is_global: false,
                summary: Some("builtins.attrNames set"),
                doc: Some(
                    "\
Return the names of the attributes in the set *set* in an
alphabetically sorted list. For instance, `builtins.attrNames { y
= 1; x = \"foo\"; }` evaluates to `[ \"x\" \"y\" ]`.\
                "
                ),
            }
        );
    }
}
