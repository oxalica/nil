use expect_test::{expect, Expect};
use syntax::parse_file;

use crate::Template;

use super::Pattern;

#[track_caller]
fn check_find(src: &str, pattern: &str, expect: Expect) {
    let parse = parse_file(src);
    assert!(parse.errors().is_empty(), "syntax error");
    let pat = Pattern::parse(pattern).expect("invalid pattern");
    let got_nodes = pat.find_iter(&parse.syntax_node()).collect::<Vec<_>>();
    let mut markers = got_nodes
        .iter()
        .flat_map(|n| [(n.text_range().start(), '<'), (n.text_range().end(), '>')])
        .collect::<Vec<_>>();
    markers.sort_by_key(|(pos, _)| *pos);

    let mut got = src.to_owned();
    for &(pos, ch) in markers.iter().rev() {
        got.insert(pos.into(), ch);
    }
    expect.assert_eq(&got);
}

#[track_caller]
fn check_replace(src: &str, pattern: &str, template: &str, expect: Expect) {
    let parse = parse_file(src);
    assert!(parse.errors().is_empty(), "syntax error");
    let pat = Pattern::parse(pattern).expect("invalid pattern");
    let templ = Template::parse(template, &pat).expect("invalid template");
    let got = pat.replace(src, &templ, &parse.syntax_node());
    expect.assert_eq(&got);
}

#[test]
fn ident() {
    // FIXME: Let bindings are not matched because they are `Attr`s.
    check_find(
        "let foo = \"foo${foo}\"; /* foo */ in foo.foo",
        "foo",
        expect![[r#"let foo = "foo${<foo>}"; /* foo */ in <foo>.foo"#]],
    );

    check_replace(
        "let foo = \"foo${foo}\"; /* foo */ in foo.foo",
        "foo",
        "bar",
        expect![[r#"let foo = "foo${bar}"; /* foo */ in bar.foo"#]],
    );
}

#[test]
fn placeholder() {
    check_find(
        "let x = 1 + 2 * 3; in (x + 1) * x",
        "$a + $b",
        expect!["let x = <1 + 2 * 3>; in (<x + 1>) * x"],
    );
    check_replace(
        "let x = 1 + 2 * 3; in (x + 1) * x",
        "$a + $b",
        "$b + $a",
        expect!["let x = 2 * 3 + 1; in (1 + x) * x"],
    );
}

#[test]
fn recursive_match() {
    check_find("1 + 2 + 3", "$a + $b", expect!["<<1 + 2 >+ 3>"]);
    // Only replace the outermost.
    check_replace("1 + 2 + 3", "$a + $b", "$b + $a", expect!["3 + (1 + 2 )"]);
}

#[test]
fn replace_paren() {
    check_replace("1 + 2 * 3", "$a + $b", "$a $b", expect!["1 (2 * 3)"]);
    check_replace("1 + 2 * 3", "$a * $b", "$a $b", expect!["1 + 2 3"]);

    check_replace(
        "let a = f 42; in f a",
        "f $a",
        "assert $a; $a",
        expect!["let a = assert 42; 42; in assert a; a"],
    );
}
