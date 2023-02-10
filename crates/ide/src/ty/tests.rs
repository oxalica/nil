use crate::tests::TestDB;
use crate::{DefDatabase, InferenceResult, Module, TyDatabase};
use expect_test::{expect, Expect};

use super::Ty;

#[track_caller]
fn check(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let module = db.module(file);
    let infer = db.infer(file);
    let ty = infer.ty_for_expr(module.entry_expr());
    let got = ty.debug().to_string();
    expect.assert_eq(&got);
}

#[track_caller]
fn check_name(name: &str, src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let module = db.module(file);
    let name = module
        .names()
        .find(|(_, n)| n.text == name)
        .expect("Name not found")
        .0;
    let infer = db.infer(file);
    let ty = infer.ty_for_name(name);
    let got = ty.debug().to_string();
    expect.assert_eq(&got);
}

fn all_types(module: &Module, infer: &InferenceResult) -> String {
    module
        .names()
        .map(|(i, name)| format!("{}: {}\n", name.text, infer.ty_for_name(i).debug()))
        .chain([format!(
            ": {}\n",
            infer.ty_for_expr(module.entry_expr()).debug(),
        )])
        .collect()
}

#[track_caller]
fn check_all(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let module = db.module(file);
    let infer = db.infer(file);
    let got = all_types(&module, &infer);
    expect.assert_eq(&got);
}

#[track_caller]
fn check_all_expect(src: &str, expect_ty: Ty, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let module = db.module(file);
    let infer = super::infer::infer_with(&db, file, Some(expect_ty));
    let got = all_types(&module, &infer);
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
    check("a: if a then 1 else 1", expect!["bool → int"]);
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

#[test]
fn external() {
    check_all_expect(
        "let a = a; in a",
        ty!(int),
        expect![[r#"
            a: int
            : int
        "#]],
    );

    check_all_expect(
        "{ stdenv }: stdenv.mkDerivation {
            name = undefined;
        }",
        ty!({
            "stdenv": {
                "mkDerivation": ({ "name": string } -> derivation),
            },
        } -> derivation),
        expect![[r#"
            stdenv: { mkDerivation: { name: string } → { args: [string], builder: string, name: string, system: string } }
            name: string
            : { stdenv: { mkDerivation: { name: string } → { args: [string], builder: string, name: string, system: string } } } → { args: [string], builder: string, name: string, system: string }
        "#]],
    );
}

#[test]
fn flake_file() {
    // Not flake.
    check_name(
        "nixpkgs",
        "
#- /default.nix
{
    outputs = { self, nixpkgs }: { };
}
        ",
        expect!["?"],
    );

    // Flake.
    check_name(
        "nixpkgs",
        "
#- /flake.nix input:nixpkgs=/nix/store/eeee
{
    outputs = { self, nixpkgs }: { };
}
              ",
        expect!["{ inputs: { }, lastModified: int, lastModifiedDate: string, narHash: string, outPath: string, outputs: { }, rev: string, revCount: int, shortRev: string, sourceInfo: { dir: string, id: string, narHash: string, owner: string, ref: string, repo: string, rev: string, submodules: bool, type: string, url: string }, submodules: bool }"],
    );
}

#[test]
fn builtins() {
    check("true", expect!["bool"]);
    check("builtins.length [ ]", expect!["int"]);
}
