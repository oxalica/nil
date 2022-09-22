#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Builtin {
    pub kind: BuiltinKind,
    pub is_global: bool,
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
            },
        );

        assert_eq!(
            ALL_BUILTINS["attrNames"],
            Builtin {
                kind: BuiltinKind::Function,
                is_global: false,
            }
        );
    }
}
