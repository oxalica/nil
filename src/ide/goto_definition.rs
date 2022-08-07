use super::NavigationTarget;
use crate::def::{AstPtr, DefDatabase, ResolveResult};
use crate::FileId;
use rowan::ast::AstNode;
use rowan::TextSize;
use syntax::{ast, match_ast, SyntaxKind, T};

pub(crate) fn goto_definition(
    db: &dyn DefDatabase,
    file_id: FileId,
    pos: TextSize,
) -> Option<Vec<NavigationTarget>> {
    let parse = db.parse(file_id);
    let tok = parse.syntax_node().token_at_offset(pos).right_biased()?;
    if !matches!(tok.kind(), T![or] | SyntaxKind::IDENT) {
        return None;
    }
    let ptr = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => Some(AstPtr::new(n.syntax())),
                ast::Name(n) => Some(AstPtr::new(n.syntax())),
                _ => None,
            }
        }
    })?;

    let source_map = db.source_map(file_id);
    let expr_id = source_map.node_expr(ptr)?;

    match db.resolve_name(file_id, expr_id)? {
        ResolveResult::NameDef(def) => {
            let name_node = source_map.name_def_node(def)?.to_node(&parse.syntax_node());
            let full_node = name_node.ancestors().find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
                )
            })?;
            Some(vec![NavigationTarget {
                file_id,
                focus_range: name_node.text_range(),
                full_range: full_node.text_range(),
            }])
        }
        ResolveResult::WithExprs(withs) => {
            let targets = withs
                .iter()
                .filter_map(|&with_expr| {
                    // with expr; body
                    // ^--^       focus
                    // ^--------^ full
                    let with_node = source_map
                        .expr_node(with_expr)
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
                .collect();
            Some(targets)
        }
        // Currently builtin names cannot "goto-definition".
        ResolveResult::Builtin(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check(fixture: &str, expect: Expect) {
        let (db, file_id, [pos]) = TestDB::single_file(fixture).unwrap();
        let src = db.file_content(file_id);
        let targets = super::goto_definition(&db, file_id, pos)
            .into_iter()
            .flatten()
            .map(|target| {
                assert!(target.full_range.contains_range(target.focus_range));
                let mut full = src[target.full_range].to_owned();
                let relative_focus = target.focus_range - target.full_range.start();
                full.insert(relative_focus.end().into(), '>');
                full.insert(relative_focus.start().into(), '<');
                full
            })
            .collect::<Vec<_>>();
        let mut got = targets.join("\n");
        // Prettify.
        if got.contains('\n') {
            got += "\n";
        }
        expect.assert_eq(&got);
    }

    #[test]
    fn not_found() {
        check("$0a", expect![]);
        check("b: $0a", expect![]);
    }

    #[test]
    fn invalid_position() {
        check("1 $0+ 2", expect![]);
        check("wi$0th 1; 2", expect![]);
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
    fn builtin() {
        check("let true = 1; in $0true && false", expect!["<true> = 1;"]);
        check("let true = 1; in true && $0false", expect![""]);
    }
}
