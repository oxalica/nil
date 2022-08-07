use crate::def::{AstPtr, DefDatabase};
use crate::{FileId, FileRange, InFile};
use rowan::ast::AstNode;
use rowan::TextSize;
use syntax::{ast, match_ast, SyntaxKind, T};

pub(crate) fn references(
    db: &dyn DefDatabase,
    file_id: FileId,
    pos: TextSize,
) -> Option<Vec<FileRange>> {
    let parse = db.parse(file_id);
    let tok = parse.syntax_node().token_at_offset(pos).right_biased()?;
    if !matches!(
        tok.kind(),
        T![with]
            | SyntaxKind::IDENT
            | T!['"']
            | SyntaxKind::STRING_ESCAPE
            | SyntaxKind::STRING_FRAGMENT
    ) {
        return None;
    }

    enum DefKind {
        Attr(AstPtr),
        With(AstPtr),
    }

    let kind = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Attr(n) => Some(DefKind::Attr(AstPtr::new(n.syntax()))),
                ast::With(n) => Some(DefKind::With(AstPtr::new(n.syntax()))),
                _ => None,
            }
        }
    })?;

    let source_map = db.source_map(file_id);
    let ref_map = db.name_reference_map(file_id);
    let refs = match kind {
        DefKind::Attr(ptr) => {
            let def = source_map.node_name_def(ptr)?;
            ref_map.def_references(def)
        }
        DefKind::With(ptr) => {
            let expr = source_map.node_expr(ptr)?;
            ref_map.with_references(expr)
        }
    };
    // When {def,with}_references returns None, it means no references,
    // not a failure.
    let refs = refs.map_or(Vec::new(), |refs| {
        refs.iter()
            .map(|&expr| {
                let ptr = source_map.expr_node(expr).expect("Id must be valid");
                InFile::new(file_id, ptr.text_range())
            })
            .collect()
    });
    Some(refs)
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;

    #[track_caller]
    fn check<const N: usize>(fixture: &str) {
        assert!(N >= 1);
        let (db, file_id, poses) = TestDB::single_file::<N>(fixture).unwrap();
        let expect = &poses[1..];
        let mut got = super::references(&db, file_id, poses[0])
            .into_iter()
            .flatten()
            .map(|file_range| file_range.value.start())
            .collect::<Vec<_>>();
        got.sort();
        assert_eq!(got, expect);
    }

    #[test]
    fn let_name() {
        check::<2>("let $0a = 1; b = 1; in $1a");
        check::<1>("let a = 1; $0b = 1; in a");
        check::<2>("let $0a.b = $1a.c; in 1");
    }

    #[test]
    fn rec_name() {
        check::<2>("rec { inherit (b) $0a; b = $1a; }");
        check::<2>("rec { inherit ($1b) a; $0b = a; }");
        check::<2>("rec { $0a.b = $1a.c; }");
    }

    #[test]
    fn special_attr() {
        check::<2>(r#"let $0" " = 1; in { inherit $1" "; }"#);
        check::<2>(r#"let "$0 " = 1; in { inherit $1" "; }"#);
        check::<2>(r#"let " " = 1; in rec { inherit $0" "; x = { inherit $1${" "}; }; }"#);
    }

    #[test]
    fn lambda_param() {
        check::<2>("$0a@{ b ? c, c ? $1a }: b");
        check::<2>("a@{ $0b ? c, c ? a }: $1b");
        check::<2>("a@{ b ? $1c, $0c ? a }: b");
    }

    #[test]
    fn with() {
        check::<1>("a: $0with {}; a");
        check::<3>("a: $0with {}; [ a $1b $2c ]");
        check::<3>("a: $0with {}; $1x + (with {}; { inherit a $2b; })");
        check::<2>("a: with {}; x + ($0with {}; { inherit a $1b; })");
    }
}
