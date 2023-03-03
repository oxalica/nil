use super::DefDatabase;
use crate::tests::TestDB;
use crate::{FlakeInfo, ModuleKind, SourceDatabase, VfsPath};
use expect_test::expect;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

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
fn source_root_referrer_graph() {
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

#- /mutual1.nix
./mutual2.nix

#- /mutual2.nix
./mutual1.nix
    ",
    )
    .unwrap();

    let sid = db.file_source_root(f["/default.nix"]);
    let source_root = db.source_root(sid);
    let graph = db.source_root_referrer_graph(sid);

    let got = source_root
        .files()
        .sorted_by_key(|&(f, _)| f)
        .map(|(referee, _)| {
            let referrers = graph.get(&referee).cloned().unwrap_or_default();
            let referee = source_root.path_for_file(referee).display();
            let referrers = referrers
                .iter()
                .map(|&f| source_root.path_for_file(f).display())
                .join(", ");
            format!("{referee} <- [{referrers}]\n")
        })
        .collect::<String>();
    expect![[r#"
        /default.nix <- [/foo/bar.nix, /bar.nix]
        /foo/bar.nix <- [/default.nix]
        /bar.nix <- [/foo/bar.nix]
        /single.nix <- []
        /mutual1.nix <- [/mutual2.nix]
        /mutual2.nix <- [/mutual1.nix]
    "#]]
    .assert_eq(&got);

    assert_eq!(
        db.module_referrers(f["/default.nix"]).into_vec(),
        vec![f["/foo/bar.nix"], f["/bar.nix"]],
    );
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

#[test]
fn source_root_flake() {
    let (db, file) = TestDB::single_file(
        "
#- /flake.nix input:nixpkgs=/nix/store/eeee
{ }
    ",
    )
    .unwrap();
    assert_eq!(
        db.source_root_flake_info(db.file_source_root(file))
            .as_deref()
            .unwrap(),
        &FlakeInfo {
            flake_file: file,
            input_store_paths: HashMap::from_iter([(
                "nixpkgs".into(),
                VfsPath::new("/nix/store/eeee"),
            )]),
        },
    );
}

#[test]
fn module_kind() {
    let (db, f) = TestDB::from_fixture(
        r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee
{
    description = "Hello";
    inputs.nixpkgs.url = "github:NixOS/nixpkgs";
    outputs = { self, nixpkgs, nix }@inputs: { };
}
        "#,
    )
    .unwrap();

    let module_kind = db.module_kind(f["/flake.nix"]);
    let ModuleKind::FlakeNix { explicit_inputs, param_inputs, .. } = &*module_kind else {
        panic!("Unexpected module kind: {module_kind:?}");
    };
    assert_eq!(
        explicit_inputs.keys().cloned().collect::<HashSet<_>>(),
        HashSet::from_iter(["nixpkgs".into()]),
    );
    assert_eq!(
        param_inputs.keys().cloned().collect::<HashSet<_>>(),
        HashSet::from_iter(["nixpkgs".into(), "nix".into()]),
    );
}
