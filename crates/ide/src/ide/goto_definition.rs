use super::NavigationTarget;
use crate::def::{AstPtr, Expr, Literal, ResolveResult};
use crate::{DefDatabase, FilePos, VfsPath};
use rowan::ast::AstNode;
use syntax::{ast, best_token_at_offset, match_ast, SyntaxKind, T};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GotoDefinitionResult {
    Path(VfsPath),
    Targets(Vec<NavigationTarget>),
}

pub(crate) fn goto_definition(
    db: &dyn DefDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<GotoDefinitionResult> {
    let parse = db.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;
    if !matches!(tok.kind(), T![or] | SyntaxKind::IDENT | SyntaxKind::PATH) {
        return None;
    }
    let ptr = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => Some(AstPtr::new(n.syntax())),
                ast::Name(n) => Some(AstPtr::new(n.syntax())),
                ast::Literal(n) => Some(AstPtr::new(n.syntax())),
                _ => None,
            }
        }
    })?;

    let source_map = db.source_map(file_id);
    let expr_id = source_map.expr_for_node(ptr)?;

    // Special case for goto-path.
    if tok.kind() == SyntaxKind::PATH {
        let module = db.module(file_id);
        let path = match &module[expr_id] {
            Expr::Literal(Literal::Path(path)) => path,
            _ => return None,
        };
        let path = path.resolve(db)?;
        return Some(GotoDefinitionResult::Path(path));
    }

    let name_res = db.name_resolution(file_id);
    let targets = match name_res.get(expr_id)? {
        &ResolveResult::Definition(name) => source_map
            .nodes_for_name(name)
            .filter_map(|ptr| {
                let name_node = ptr.to_node(&parse.syntax_node());
                let full_node = name_node.ancestors().find(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
                    )
                })?;
                Some(NavigationTarget {
                    file_id,
                    focus_range: name_node.text_range(),
                    full_range: full_node.text_range(),
                })
            })
            .collect(),
        ResolveResult::WithExprs(withs) => {
            withs
                .iter()
                .filter_map(|&with_expr| {
                    // with expr; body
                    // ^--^       focus
                    // ^--------^ full
                    let with_node = source_map
                        .node_for_expr(with_expr)
                        .expect("WithExprs must be valid")
                        .to_node(&parse.syntax_node());
                    let with_node = ast::With::cast(with_node).expect("WithExprs must be valid");
                    let with_token_range = with_node.with_token()?.text_range();
                    let with_header_end = with_node
                        .semicolon_token()
                        .map_or_else(|| with_node.syntax().text_range(), |tok| tok.text_range());
                    let with_header = with_token_range.cover(with_header_end);
                    Some(NavigationTarget {
                        file_id,
                        focus_range: with_token_range,
                        full_range: with_header,
                    })
                })
                .collect()
        }
        // Currently builtin names cannot "goto-definition".
        ResolveResult::Builtin(_) => return None,
    };

    Some(GotoDefinitionResult::Targets(targets))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check_no(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(goto_definition(&db, f[0]), None);
    }

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let mut got = match goto_definition(&db, f[0]).expect("No definition") {
            GotoDefinitionResult::Path(path) => format!("file://{}", path.as_str()),
            GotoDefinitionResult::Targets(targets) => {
                assert!(!targets.is_empty());
                targets
                    .into_iter()
                    .map(|target| {
                        assert!(target.full_range.contains_range(target.focus_range));
                        let src = db.file_content(target.file_id);
                        let mut full = src[target.full_range].to_owned();
                        let relative_focus = target.focus_range - target.full_range.start();
                        full.insert(relative_focus.end().into(), '>');
                        full.insert(relative_focus.start().into(), '<');
                        full
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            }
        };
        // Prettify.
        if got.contains('\n') {
            got += "\n";
        }
        expect.assert_eq(&got);
    }

    #[test]
    fn not_found() {
        check_no("$0a");
        check_no("b: $0a");
    }

    #[test]
    fn invalid_position() {
        check_no("1 $0+ 2");
        check_no("wi$0th 1; 2");
    }

    #[test]
    fn lambda_param() {
        check("a: (a: (a $0a)) 1", expect!["<a>: (a a)"]);
        check("x: (a: (a $0x)) 1", expect!["<x>: (a: (a x)) 1"]);
        check("a: (a@{ x }: (a $0a)) 1", expect!["<a>@{ x }: (a a)"]);
        check("a: ({ x ? $0a }@a: a) 1", expect!["{ x ? a }@<a>: a"]);
        check("a: ({ x ? $0x }@a: a) 1", expect!["{ <x> ? x }@a: a"]);
    }

    #[test]
    fn with_env() {
        check("with 1; let a = 1; in with 2; $0a", expect!["<a> = 1;"]);
        check(
            "with 1; let a = 1; in with 2; $0b",
            expect![[r#"
                <with> 2;
                <with> 1;
            "#]],
        );
    }

    #[test]
    fn bindings() {
        check(
            "let a = a; in rec { inherit a; b = $0a; }",
            expect!["inherit <a>;"],
        );
        check(
            "let a = a; in rec { inherit $0a; b = a; }",
            expect!["<a> = a;"],
        );
        check(
            "let a = $0a; in rec { inherit a; b = a; }",
            expect!["<a> = a;"],
        );
    }

    #[test]
    fn left_and_right() {
        check("let a = 1; in $0a ", expect!["<a> = 1;"]);
        check("let a = 1; in a$0 ", expect!["<a> = 1;"]);
        check("let a = 1; in 0+$0a+0", expect!["<a> = 1;"]);
        check("let a = 1; in 0+a$0+0", expect!["<a> = 1;"]);
    }

    #[test]
    fn merged_binding() {
        check(
            "let a.a = 1; a.b = 2; a = { c = 3; }; in $0a",
            expect![[r#"
                <a>.a = 1;
                <a>.b = 2;
                <a> = { c = 3; };
            "#]],
        );
        check(
            "rec { b = $0a; a = { a = 1; }; a = { a = 2; }; }",
            expect![[r#"
                <a> = { a = 1; };
                <a> = { a = 2; };
            "#]],
        );
    }

    #[test]
    fn builtin() {
        check("let true = 1; in $0true && false", expect!["<true> = 1;"]);
        check_no("let true = 1; in true && $0false");
    }

    #[test]
    fn path() {
        check("1 + $0./.", expect!["file://"]);
        check(
            "
#- /default.nix
import $0./bar.nix

#- /bar.nix
hello
            ",
            expect!["file:///bar.nix"],
        );
    }
}
