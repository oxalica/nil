use crate::def::lower::lower;
use crate::source::{FileId, InFile};
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
