use crate::{DefDatabase, Diagnostic, FileId};

/// Get all diagnostics for a file.
pub(crate) fn diagnostics(db: &dyn DefDatabase, file: FileId) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    // Parsing.
    let parse = db.parse(file);
    diags.extend(parse.errors().iter().map(|&err| Diagnostic::from(err)));

    // Lowering.
    let source_map = db.source_map(file);
    diags.extend(source_map.diagnostics().iter().cloned());

    // Name resolution.
    diags.extend(db.name_resolution(file).to_diagnostics(db, file));

    // Liveness check.
    let liveness = db.liveness_check(file);
    diags.extend(liveness.to_diagnostics(db, file));

    diags
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check(fixture: &str, expect: Expect) {
        let (db, file_id) = TestDB::single_file(fixture).unwrap();
        let diags = super::diagnostics(&db, file_id);
        assert!(!diags.is_empty());
        let mut got = diags
            .iter()
            .map(|d| d.debug_display().to_string())
            .collect::<Vec<_>>()
            .join("\n");
        if got.contains('\n') {
            got.push('\n');
        }
        expect.assert_eq(&got);
    }

    #[test]
    fn syntax_error() {
        check("1 == 2 == 3", expect!["7..9: SyntaxError(MultipleNoAssoc)"]);
    }

    #[test]
    fn lower_error() {
        check(
            "{ a = 1; a = 2; }",
            expect![[r#"
                9..10: DuplicatedKey
                    2..3: Previously defined here
            "#]],
        );
    }

    #[test]
    fn name_resolution() {
        check("a", expect!["0..1: UndefinedName"]);
    }

    #[test]
    fn liveness() {
        check(
            "let a = a; b = 1; in with 1; b + rec { }",
            expect![[r#"
                4..5: UnusedBinding
                21..28: UnusedWith
                33..36: UnusedRec
            "#]],
        );
    }

    #[test]
    fn deterministic_order() {
        check(
            "let a = 1; b = 2; c = 3; d = 4; in 0",
            expect![[r#"
                4..5: UnusedBinding
                11..12: UnusedBinding
                18..19: UnusedBinding
                25..26: UnusedBinding
            "#]],
        );
    }
}
