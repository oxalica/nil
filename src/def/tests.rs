use super::DefDatabase;
use crate::tests::FixtureExt;
use crate::RootDatabase;
use expect_test::expect;

#[test]
fn module_basic() {
    let (db, root_id) = RootDatabase::from_single_file(
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
                        "foo",
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
}
