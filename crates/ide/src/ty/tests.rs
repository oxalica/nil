use crate::tests::TestDB;
use crate::{DefDatabase, TyDatabase};
use expect_test::{expect, Expect};

fn check(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let module = db.module(file);
    let infer = db.infer(file);
    let ty = infer.ty_for_expr(module.entry_expr());
    let got = infer.debug_ty(ty).to_string();
    expect.assert_eq(&got);
}

fn check_all(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let module = db.module(file);
    let infer = db.infer(file);
    let got = module
        .names()
        .map(|(i, name)| format!("{}: {}\n", name.text, infer.debug_ty(infer.ty_for_name(i))))
        .chain([format!(
            ": {}\n",
            infer.debug_ty(infer.ty_for_expr(module.entry_expr()))
        )])
        .collect::<String>();
    expect.assert_eq(&got);
}

#[test]
fn literal() {
    check("42", expect!["int"]);
    check("1.2", expect!["float"]);
    check("./.", expect!["path"]);
    check(r#""foo""#, expect!["string"]);
}

#[test]
fn simple_operator() {
    check(r#""a" + "b""#, expect!["string"]);
    check("1 + 1", expect!["int"]);
    // TODO
    // check("1 - 1.2", expect!["float"]);
    check("1.2 * 1", expect!["float"]);
    check("1.2 / 1.2", expect!["float"]);
    check("1 == 2", expect!["bool"]);
    check("1 < 2", expect!["bool"]);
    check("a -> b", expect!["bool"]);

    check("! (1 == 1)", expect!["bool"]);
    check("-1", expect!["int"]);
    check("-1.2", expect!["float"]);
}

#[test]
fn collection_operator() {
    check_all(
        "let a = []; b = [ 1 c ]; c = c; in a ++ b",
        expect![[r#"
            a: [int]
            b: [int]
            c: int
            : [int]
        "#]],
    );
    check_all(
        "let a.a = 1; b.b = 2; in a // b",
        expect![[r#"
            a: { a: int, b: int }
            a: int
            b: { a: int, b: int }
            b: int
            : { a: int, b: int }
        "#]],
    );
}

#[test]
fn let_in() {
    check_all(
        "let a = c + 1; b = 1; c = b + 1; in a",
        expect![[r#"
            a: int
            b: int
            c: int
            : int
        "#]],
    );
}

#[test]
fn recursive() {
    check("let a = a; in a", expect!["?"]);
    check("let a = a + 1; in a", expect!["int"]);
    check("let a = [a]; in a", expect!["[?]"]);

    check("let a = b + 1; b = a + 1; in a", expect!["int"]);
    check(
        "let a = { b = 1; inherit a; }; in a",
        expect!["{ a: ?, b: int }"],
    );
}

#[test]
fn if_then_else() {
    check(
        "if 1 == 2 then { a = 1; } else { b = 1; }",
        expect!["{ a: int, b: int }"],
    );
}

#[test]
fn lambda() {
    check("a: a", expect!["? → ?"]);
    check("(a: a) 1", expect!["int"]);

    check("{ }: 1", expect!["{ } → int"]);
    check_all(
        "{ a ? b, b ? 42 }@c: b",
        expect![[r#"
            c: { a: int, b: int }
            a: int
            b: int
            : { a: int, b: int } → int
        "#]],
    );
}

#[test]
fn select() {
    check("a: a.b.c", expect!["{ b: { c: ? } } → ?"]);
    check("a: a.b.c or 42", expect!["{ b: { c: int } } → int"]);

    check_all(
        "a: b: a.${b}",
        expect![[r#"
            a: { }
            b: string
            : { } → string → ?
        "#]],
    );
}
