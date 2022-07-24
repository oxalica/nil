use super::DefDatabase;
use crate::def::AstPtr;
use crate::tests::TestDB;
use expect_test::{expect, Expect};
use rowan::ast::AstNode;
use syntax::ast;

#[track_caller]
fn check_scopes(src: &str, expect: Expect) {
    let (db, file_id, pos) = TestDB::from_file_with_pos(src);
    let ptr = AstPtr::new(db.node_at::<ast::Expr>(file_id, pos).syntax());

    let module = db.module(file_id);
    let source_map = db.source_map(file_id);
    let expr_id = source_map.expr_map[&ptr];
    let scopes = db.scopes(file_id);
    let scope_id = scopes.scope_by_expr(expr_id).expect("No scope data");

    // "innermost var abc | middle var | outmost var"
    let scope_defs = scopes
        .ancestors(scope_id)
        .filter_map(|scope| {
            let mut names = scope
                .name_defs()
                .map(|def_id| module[def_id].name.clone())
                .collect::<Vec<_>>();
            if names.is_empty() {
                return None;
            }
            names.sort();
            Some(names.join(" "))
        })
        .collect::<Vec<_>>();
    let got = scope_defs.join(" | ");

    expect.assert_eq(&got);
}

#[test]
fn top_level() {
    check_scopes(r"42$0", expect![""]);
}

#[test]
fn lambda() {
    check_scopes(r"(a: b: (c: 0) 42$0 (d: 0)) (e: 0)", expect!["b | a"]);
    check_scopes(r"{ a, b ? c, ... }@d: $0x (y: x)", expect!["a b d"]);
    check_scopes(r"a: { a, b ? $0c, ... }@d: y: a", expect!["a b d | a"]);
}
