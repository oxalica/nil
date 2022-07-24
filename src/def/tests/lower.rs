use crate::base::{FileId, InFile};
use crate::def::lower::lower;
use expect_test::{expect, Expect};
use std::fmt::Write;
use syntax::parse_file;

fn check_lower(src: &str, expect: Expect) {
    let parse = parse_file(src);
    let (module, _source_map) = lower(InFile::new(FileId(0), parse.root()));
    let mut got = String::new();
    for (i, e) in module.exprs.iter() {
        writeln!(got, "{}: {:?}", i.into_raw(), e).unwrap();
    }
    expect.assert_eq(&got);
}

#[test]
fn literal() {
    check_lower(
        "42",
        expect![[r#"
            0: Literal(Int(42))
        "#]],
    );
    check_lower(
        "1.2e3",
        expect![[r#"
            0: Literal(Float(OrderedFloat(1200.0)))
        "#]],
    );
    check_lower(
        "a:b",
        expect![[r#"
            0: Literal(String("a:b"))
        "#]],
    );
}

#[test]
fn path() {
    check_lower(
        "./.",
        expect![[r#"
            0: Literal(Path(Path { anchor: Relative(FileId(0)), supers: 0, raw_segments: "" }))
        "#]],
    );
    check_lower(
        "../.",
        expect![[r#"
            0: Literal(Path(Path { anchor: Relative(FileId(0)), supers: 1, raw_segments: "" }))
        "#]],
    );
    check_lower(
        "../a/../../.b/./c",
        expect![[r#"
            0: Literal(Path(Path { anchor: Relative(FileId(0)), supers: 2, raw_segments: ".b/c" }))
        "#]],
    );
    check_lower(
        "/../a/../../.b/./c",
        expect![[r#"
            0: Literal(Path(Path { anchor: Absolute, supers: 0, raw_segments: ".b/c" }))
        "#]],
    );
    check_lower(
        "~/../a/../../.b/./c",
        expect![[r#"
            0: Literal(Path(Path { anchor: Home, supers: 2, raw_segments: ".b/c" }))
        "#]],
    );
    check_lower(
        "<p/../a/../../.b/./c>",
        expect![[r#"
            0: Literal(Path(Path { anchor: Search("p"), supers: 2, raw_segments: ".b/c" }))
        "#]],
    );
}

#[test]
fn lambda() {
    check_lower(
        "a: 0",
        expect![[r#"
            0: Literal(Int(0))
            1: Lambda(Some(Idx::<NameDef>(0)), None, Idx::<Expr>(0))
        "#]],
    );
    check_lower(
        "{ }: 0",
        expect![[r#"
            0: Literal(Int(0))
            1: Lambda(None, Some(Pat { fields: [], ellipsis: false }), Idx::<Expr>(0))
        "#]],
    );
    check_lower(
        "a@{ ... }: 0",
        expect![[r#"
            0: Literal(Int(0))
            1: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [], ellipsis: true }), Idx::<Expr>(0))
        "#]],
    );
    check_lower(
        "{ }@a: 0",
        expect![[r#"
            0: Literal(Int(0))
            1: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [], ellipsis: false }), Idx::<Expr>(0))
        "#]],
    );
    check_lower(
        "{ a, b ? 0, ... }: 0",
        expect![[r#"
            0: Literal(Int(0))
            1: Literal(Int(0))
            2: Lambda(None, Some(Pat { fields: [(Some(Idx::<NameDef>(0)), None), (Some(Idx::<NameDef>(1)), Some(Idx::<Expr>(0)))], ellipsis: true }), Idx::<Expr>(1))
        "#]],
    );
    check_lower(
        "{ a ? 0, b }@c: 0",
        expect![[r#"
            0: Literal(Int(0))
            1: Literal(Int(0))
            2: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [(Some(Idx::<NameDef>(1)), Some(Idx::<Expr>(0))), (Some(Idx::<NameDef>(2)), None)], ellipsis: false }), Idx::<Expr>(1))
        "#]],
    );
}
