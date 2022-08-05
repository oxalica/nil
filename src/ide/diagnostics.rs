use crate::def::DefDatabase;
use crate::{Diagnostic, FileId};

const MAX_DIAGNOSTIC_CNT: usize = 128;

pub(crate) fn diagnostics(db: &dyn DefDatabase, file: FileId) -> Vec<Diagnostic> {
    let parse = db.parse(file).value;
    let module = db.module(file);
    parse
        .errors()
        .iter()
        .map(|&err| Diagnostic::from(err))
        .chain(module.diagnostics().iter().cloned())
        .take(MAX_DIAGNOSTIC_CNT)
        .collect()
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
}
