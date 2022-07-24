use super::DefDatabase;
use crate::tests::TestDB;
use expect_test::expect;

mod lower;
mod scope;

#[test]
fn source_map() {
    let (db, root_id) = TestDB::from_file("foo 123");

    let source_map = db.source_map(root_id);
    let mut expr_map = source_map.expr_map.iter().collect::<Vec<_>>();
    expr_map.sort_by_key(|(_, id)| id.into_raw());
    let ptrs = expr_map.iter().map(|(ptr, _)| ptr).collect::<Vec<_>>();
    expect![[r#"
        [
            SyntaxNodePtr {
                kind: REF,
                range: 0..3,
            },
            SyntaxNodePtr {
                kind: LITERAL,
                range: 4..7,
            },
            SyntaxNodePtr {
                kind: APPLY,
                range: 0..7,
            },
        ]
    "#]]
    .assert_debug_eq(&ptrs);
}
