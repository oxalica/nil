use crate::{DefDatabase, Diagnostic, DiagnosticKind, FileId};
use rowan::ast::AstNode;
use syntax::ast;

pub(crate) fn diagnostics(db: &dyn DefDatabase, file: FileId) -> Vec<Diagnostic> {
    let parse = db.parse(file);
    let module = db.module(file);

    let mut diags = Vec::new();

    // Parsing.
    diags.extend(parse.errors().iter().map(|&err| Diagnostic::from(err)));

    // Lowering.
    diags.extend(module.diagnostics().iter().cloned());

    // Liveness check.
    let liveness = db.liveness_check(file);
    let source_map = db.source_map(file);
    diags.extend(liveness.unused_name_defs().iter().map(|&def| Diagnostic {
        range: source_map.name_def_node(def).unwrap().text_range(),
        kind: DiagnosticKind::UnusedBinding,
    }));
    diags.extend(liveness.unused_withs().iter().map(|&expr| {
        let ptr = source_map.expr_node(expr).expect("Must be With");
        let node = ast::With::cast(ptr.to_node(&parse.syntax_node())).expect("Must be With");
        let with_token_range = node
            .with_token()
            .expect("With must has `with` token")
            .text_range();
        let with_header_range = node.semicolon_token().map_or_else(
            || node.syntax().text_range(),
            |tok| tok.text_range().cover(with_token_range),
        );
        Diagnostic {
            range: with_header_range,
            kind: DiagnosticKind::UnusedWith,
        }
    }));
    diags.extend(liveness.unused_recs().iter().map(|&expr| {
        let ptr = source_map.expr_node(expr).expect("Must be Attrset");
        let node = ast::AttrSet::cast(ptr.to_node(&parse.syntax_node())).expect("Must be Attrset");
        let rec_range = node
            .rec_token()
            .map_or_else(|| node.syntax().text_range(), |tok| tok.text_range());
        Diagnostic {
            range: rec_range,
            kind: DiagnosticKind::UnusedRec,
        }
    }));

    diags
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check(fixture: &str, expect: Expect) {
        let (db, file_id, []) = TestDB::single_file(fixture).unwrap();
        let diags = super::diagnostics(&db, file_id);
        assert!(!diags.is_empty());
        let got = diags
            .iter()
            .map(|d| d.to_string() + "\n")
            .collect::<Vec<_>>()
            .join("");
        expect.assert_eq(&got);
    }

    #[test]
    fn syntax_error() {
        check(
            "1 == 2 == 3",
            expect![[r#"
                Invalid usage of no-associative operators at 7..9
            "#]],
        );
    }

    #[test]
    fn lower_error() {
        check(
            "{ a = 1; a = 2; }",
            expect![[r#"
                Duplicated name definition at 2..3
                Duplicated name definition at 9..10
            "#]],
        );
    }

    #[test]
    fn liveness() {
        check(
            "let a = a; b = 1; in with 1; b + rec { }",
            expect![[r#"
                Unused binding at 4..5
                Unused `with` at 21..28
                Unused `rec` at 33..36
            "#]],
        );
    }
}
