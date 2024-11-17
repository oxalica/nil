use crate::def::{AstPtr, ResolveResult};
use crate::{DefDatabase, FilePos, FileRange};
use syntax::ast::{self, AstNode};
use syntax::{best_token_at_offset, SyntaxKind, T};

enum DefKind {
    Attr(AstPtr),
    With(AstPtr),
}

/// Return all references of this expression.
pub(crate) fn references(
    db: &dyn DefDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<Vec<FileRange>> {
    let parse = db.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;

    let kind = match tok.kind() {
        T![with] => {
            let n = tok.parent().and_then(ast::With::cast)?;
            DefKind::With(AstPtr::new(n.syntax()))
        }
        SyntaxKind::IDENT => DefKind::Attr(AstPtr::new(&tok.parent()?)),
        T!['"'] | SyntaxKind::STRING_ESCAPE | SyntaxKind::STRING_FRAGMENT => {
            let tok = tok.parent().and_then(ast::String::cast)?;
            DefKind::Attr(AstPtr::new(tok.syntax()))
        }
        _ => return None,
    };

    let source_map = db.source_map(file_id);
    let nameres = db.name_resolution(file_id);
    let nameref = db.name_reference(file_id);
    let refs = match kind {
        DefKind::Attr(ptr) => {
            // If this is not a name definition, but a usage. We lookup its definition for the
            // query. This is covered by the test `on_usage`.
            let name = source_map.name_for_node(ptr).or_else(|| {
                let expr = source_map.expr_for_node(ptr)?;
                let ResolveResult::Definition(name) = nameres.get(expr)? else {
                    return None;
                };
                Some(*name)
            })?;
            nameref.name_references(name)
        }
        DefKind::With(ptr) => {
            let expr = source_map.expr_for_node(ptr)?;
            nameref.with_references(expr)
        }
    };
    // When {name,with}_references returns None, it means no references,
    // not a failure.
    let refs = refs.map_or(Vec::new(), |refs| {
        refs.iter()
            .map(|&expr| {
                let ptr = source_map.node_for_expr(expr).expect("Id must be valid");
                FileRange::new(file_id, ptr.text_range())
            })
            .collect()
    });
    Some(refs)
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;

    #[track_caller]
    fn check(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert!(!f.markers().is_empty());
        let expect = f.markers()[1..].iter().map(|p| p.pos).collect::<Vec<_>>();
        let mut got = super::references(&db, f[0])
            .into_iter()
            .flatten()
            .map(|frange| frange.range.start())
            .collect::<Vec<_>>();
        got.sort();
        assert_eq!(got, expect);
    }

    #[test]
    fn let_name() {
        check("let $0a = 1; b = 1; in $1a");
        check("let a = 1; $0b = 1; in a");
        check("let $0a.b = $1a.c; in 1");
    }

    #[test]
    fn rec_name() {
        check("rec { inherit (b) $0a; b = $1a; }");
        check("rec { inherit ($1b) a; $0b = a; }");
        check("rec { $0a.b = $1a.c; }");
    }

    #[test]
    fn special_attr() {
        check(r#"let $0" " = 1; in { inherit $1" "; }"#);
        check(r#"let "$0 " = 1; in { inherit $1" "; }"#);

        // The location of static `${}` attrs are the whole `${}`. Not the inner string.
        // This behavior is relied by `ide::rename`.
        check(r#"let " " = 1; in rec { inherit $0" "; x = { inherit $1${" "}; }; }"#);
    }

    #[test]
    fn lambda_param() {
        check("$0a@{ b ? c, c ? $1a }: b");
        check("a@{ $0b ? c, c ? a }: $1b");
        check("a@{ b ? $1c, $0c ? a }: b");
    }

    #[test]
    fn with_attr() {
        check("a: $0with {}; a");
        check("a: $0with {}; [ a $1b $2c ]");
        check("a: $0with {}; $1x + (with {}; { inherit a $2b; })");
        check("a: with {}; x + ($0with {}; { inherit a $1b; })");
    }

    #[test]
    fn on_usage() {
        check("let x = 1; y = $0$1x; z = $2x; in z");

        // Attributes from `with` should still return nothing. It would be surprising to show other
        // unrelated attributes as "references".
        check("with {}; $0a + b");
    }
}
