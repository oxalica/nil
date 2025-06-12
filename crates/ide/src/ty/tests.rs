use std::collections::HashMap;
use std::sync::Arc;

use crate::tests::TestDB;
use crate::{
    DefDatabase, FlakeGraph, FlakeInfo, InferenceResult, Module, SourceDatabase, TyDatabase,
};
use expect_test::{expect, Expect};
use nix_interop::flake_output::{FlakeOutput, Type};

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
fn pipe() {
    check("f: 1 |> f", expect!["(int → ?) → ?"]);
    check("f: f <| 1", expect!["(int → ?) → ?"]);
}

#[test]
fn cur_pos() {
    // Cannot be shadowed.
    check(
        "let __curPos = 1; in __curPos",
        expect!["{ column: int, file: string, line: int }"],
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
fn rest_type_decl_site() {
    check_name(
        "bar",
        r"
#- /flake.nix
{
    outputs = { self }: {
        apps.x86_64-linux = {
            foo = let bar = bar; in bar;
        };
    };
}
        ",
        expect!["{ program: string, type: string }"],
    );
}

#[test]
fn rest_type_use_site_missing() {
    check_name(
        "export",
        r#"
#- /flake.nix
rec {
    inputs.a.url = "url:url";
    outputs = { ... }: {
        export = inputs.a.inputs.missing.follows;
    };
}
        "#,
        expect!["string"],
    );
}

#[test]
fn rest_type_use_site_dynamic() {
    check_name(
        "export",
        r#"
#- /flake.nix
rec {
    inputs.a.url = "url:url";
    outputs = { ... }: {
        export = inputs.a.inputs.${builtins.getEnv "dynamic"}.follows;
    };
}
        "#,
        expect!["string"],
    );
}

#[test]
fn builtins() {
    check("true", expect!["bool"]);
    check("builtins.length [ ]", expect!["int"]);
    check("builtins.readDir ./.", expect!["{ …: string }"]);
    check("(builtins.readDir ./.).foo", expect!["string"]);
}

#[test]
fn inputs_with_self() {
    check_name(
        "foo",
        r"
#- /flake.nix
{
    inputs.self.foo = 1;
}
        ",
        expect!["int"],
    );
}

#[test]
fn input_flake_ty() {
    let src = r#"
#- /flake.nix
{
    inputs.nixpkgs = "...";
    outputs = { self, nixpkgs }: {
        export_output = nixpkgs.outputs;
        export_pkg_name = nixpkgs.legacyPackages.x86_64-linux.hello.name;
    };
}
    "#;

    let nixpkgs_output = FlakeOutput::Attrset(HashMap::from_iter([(
        "legacyPackages".into(),
        FlakeOutput::Attrset(HashMap::from_iter([(
            "x86_64-linux".into(),
            FlakeOutput::Attrset(HashMap::from_iter([(
                "hello".into(),
                FlakeOutput::Leaf(nix_interop::flake_output::Leaf {
                    type_: Type::Derivation,
                    name: None,
                    description: None,
                }),
            )])),
        )])),
    )]));

    let expect_output =
        expect!["{ legacyPackages: { x86_64-linux: { hello: { args: [string], builder: string, name: string, system: string } }, …: { hello: { args: [string], builder: string, name: string, system: string } } } }"];

    let (mut db, file) = TestDB::single_file(src).unwrap();
    let sid = db.file_source_root(file);
    db.set_flake_graph(Arc::new(FlakeGraph {
        nodes: HashMap::from_iter([(
            sid,
            FlakeInfo {
                flake_file: file,
                input_store_paths: HashMap::new(),
                input_flake_outputs: HashMap::from_iter([("nixpkgs".into(), nixpkgs_output)]),
            },
        )]),
    }));
    let ty_for_name = |name: &str| {
        let name = db
            .module(file)
            .names()
            .find(|(_, n)| n.text == name)
            .expect("Name not found")
            .0;
        db.infer(file).ty_for_name(name).debug().to_string()
    };
    expect_output.assert_eq(&ty_for_name("export_output"));
    assert_eq!(ty_for_name("export_pkg_name"), "string");
}
