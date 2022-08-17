use crate::{DefDatabase, Diagnostic, FileId};

pub(crate) fn diagnostics(db: &dyn DefDatabase, file: FileId) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    // Parsing.
    let parse = db.parse(file);
    diags.extend(parse.errors().iter().map(|&err| Diagnostic::from(err)));

    // Lowering.
    let module = db.module(file);
    diags.extend(module.diagnostics().iter().cloned());

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
        let got = diags
            .iter()
            .map(|d| d.debug_to_string() + "\n")
            .collect::<Vec<_>>()
            .join("");
        expect.assert_eq(&got);
    }

    #[test]
    fn syntax_error() {
        check(
            "1 == 2 == 3",
            expect![[r#"
                7..9: Invalid usage of no-associative operators
            "#]],
        );
    }

    #[test]
    fn lower_error() {
        check(
            "{ a = 1; a = 2; }",
            expect![[r#"
                2..3: Duplicated name definition
                9..10: Duplicated name definition
            "#]],
        );
    }

    #[test]
    fn name_resolution() {
        check(
            "a",
            expect![[r#"
                0..1: Undefined name
            "#]],
        );
    }

    #[test]
    fn liveness() {
        check(
            "let a = a; b = 1; in with 1; b + rec { }",
            expect![[r#"
                4..5: Unused binding
                21..28: Unused `with`
                33..36: Unused `rec`
            "#]],
        );
    }
}
