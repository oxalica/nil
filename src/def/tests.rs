use super::DefDatabase;
use crate::tests::TestDB;
use expect_test::expect;

#[test]
fn module_basic() {
    let (db, root_id) = TestDB::from_file(
        r#"
        foo 123 ./bar.nix
        "#,
    );
    expect![[r#"
        Module {
            exprs: Arena {
                len: 5,
                data: [
                    Ident(
                        Name(
                            "foo",
                        ),
                    ),
                    Literal(
                        Int(
                            123,
                        ),
                    ),
                    Apply(
                        Idx::<Expr>(0),
                        Idx::<Expr>(1),
                    ),
                    Literal(
                        Path(
                            Path {
                                anchor: Relative(
                                    FileId(
                                        0,
                                    ),
                                ),
                                raw_segments: "./bar.nix",
                            },
                        ),
                    ),
                    Apply(
                        Idx::<Expr>(2),
                        Idx::<Expr>(3),
                    ),
                ],
            },
        }
    "#]]
    .assert_debug_eq(&db.module(root_id));

    let source_map = db.source_map(root_id);
    let mut expr_map = source_map.expr_map.iter().collect::<Vec<_>>();
    expr_map.sort_by_key(|(_, id)| id.into_raw());
    let ptrs = expr_map.iter().map(|(ptr, _)| ptr).collect::<Vec<_>>();
    expect![[r#"
        [
            SyntaxNodePtr {
                kind: NODE_IDENT,
                range: 9..12,
            },
            SyntaxNodePtr {
                kind: NODE_LITERAL,
                range: 13..16,
            },
            SyntaxNodePtr {
                kind: NODE_APPLY,
                range: 9..16,
            },
            SyntaxNodePtr {
                kind: NODE_LITERAL,
                range: 17..26,
            },
            SyntaxNodePtr {
                kind: NODE_APPLY,
                range: 9..26,
            },
        ]
    "#]]
    .assert_debug_eq(&ptrs);
}
