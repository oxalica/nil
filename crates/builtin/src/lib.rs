#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Builtin {
    pub kind: BuiltinKind,
    pub is_global: bool,
    pub summary: &'static str,
    pub doc: Option<&'static str>,
    pub impure_only: bool,
    pub experimental_feature: Option<&'static str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinKind {
    Const,
    Function,
    Attrset,
}

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

        // We do not want to assert on the exact details of the documentation.
        assert!(ALL_BUILTINS["attrNames"].doc.is_some());
        assert_eq!(
            ALL_BUILTINS["attrNames"],
            Builtin {
                kind: BuiltinKind::Function,
                is_global: false,
                summary: "`builtins.attrNames set`",
                doc: ALL_BUILTINS["attrNames"].doc,
                impure_only: false,
                experimental_feature: None,
            }
        );
    }
}
