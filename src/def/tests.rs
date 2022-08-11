use super::DefDatabase;
use crate::tests::TestDB;
use crate::SourceDatabase;
use expect_test::expect;
use std::collections::HashSet;

#[test]
fn source_map() {
    let (db, root) = TestDB::single_file("foo 123").unwrap();

    let source_map = db.source_map(root);
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

#[test]
fn module_references() {
    let (db, f) = TestDB::from_fixture(
        "
#- /default.nix
./foo/bar.nix

#- /foo/bar.nix
baz/../../bar.nix + ../default.nix

#- /bar.nix
./.

#- /single.nix
42
    ",
    )
    .unwrap();

    let asserts = [
        ("/default.nix", &["/foo/bar.nix"][..]),
        ("/foo/bar.nix", &["/bar.nix", "/default.nix"]),
        ("/bar.nix", &["/default.nix"]),
        ("/single.nix", &[]),
    ];

    for (src, refs) in asserts {
        let got = &*db.module_references(f[src]);
        let expect = refs.iter().map(|path| f[*path]).collect::<HashSet<_>>();
        assert_eq!(got, &expect, "Module {:?} should reference {:?}", src, refs);
    }
}

#[test]
fn source_root_closure() {
    let (db, f) = TestDB::from_fixture(
        "
#- /default.nix
./foo/bar.nix

#- /foo/bar.nix
baz/../../bar.nix + ../default.nix

#- /bar.nix
./.

#- /single.nix
42
    ",
    )
    .unwrap();

    assert_eq!(
        *db.source_root_closure(db.file_source_root(f["/default.nix"])),
        [f["/default.nix"], f["/foo/bar.nix"], f["/bar.nix"]]
            .into_iter()
            .collect::<HashSet<_>>(),
    );
}
