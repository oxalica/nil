#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A nix builtin.
pub struct Builtin {
    pub kind: BuiltinKind,
    /// Whether the builtin is globally available in code.
    pub is_global: bool,
    pub summary: &'static str,
    pub doc: Option<&'static str>,
    /// Can only be used in impure code.
    pub impure_only: bool,
    /// Whether this builtin is experimental.
    pub experimental_feature: Option<&'static str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinKind {
    Const,
    Function,
    Attrset,
}

/// List of all builtins.
#[allow(clippy::all)]
pub static ALL_BUILTINS: phf::Map<&'static str, Builtin> =
    include!(concat!(env!("OUT_DIR"), "/generated.expr"));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanity() {
        assert!(matches!(
            ALL_BUILTINS["true"],
            Builtin {
                kind: BuiltinKind::Const,
                is_global: true,
                summary: "`builtins.true`",
                doc: _,
                impure_only: false,
                experimental_feature: None,
            }
        ));

        assert_eq!(
            ALL_BUILTINS["attrNames"],
            Builtin {
                kind: BuiltinKind::Function,
                is_global: false,
                summary: "`builtins.attrNames set`",
                doc: Some(
                    "\
Return the names of the attributes in the set *set* in an
alphabetically sorted list. For instance, `builtins.attrNames { y
= 1; x = \"foo\"; }` evaluates to `[ \"x\" \"y\" ]`.\
                "
                ),
                impure_only: false,
                experimental_feature: None,
            }
        );
    }
}
