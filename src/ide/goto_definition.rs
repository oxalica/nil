use super::NavigationTarget;
use crate::{
    def::{AstPtr, DefDatabase, ResolveResult},
    FileId,
};
use rowan::{ast::AstNode, TextSize};
use syntax::{ast, match_ast, SyntaxKind, T};

pub(crate) fn goto_definition(
    db: &dyn DefDatabase,
    file_id: FileId,
    pos: TextSize,
) -> Option<NavigationTarget> {
    let parse = db.parse(file_id).value;
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

    let (focus_range, full_range) = match db.resolve_name(file_id, expr_id)? {
        ResolveResult::NameDef(def) => {
            let name_node = source_map.name_def_node(def)?.to_node(&parse.syntax_node());
            let full_node = name_node.ancestors().find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
                )
            })?;
            (name_node.text_range(), full_node.text_range())
        }
        ResolveResult::WithEnv(env) => {
            // with expr; body
            // ^--^       focus
            // ^--------^ full
            let env_node = source_map.expr_node(env)?.to_node(&parse.syntax_node());
            let with_node = ast::With::cast(env_node.parent()?)?;
            let with_token_range = with_node.with_token()?.text_range();
            let with_header_end = with_node
                .semicolon_token()
                .map_or_else(|| env_node.text_range(), |tok| tok.text_range());
            let with_header = with_token_range.cover(with_header_end);
            (with_token_range, with_header)
        }
        // Currently builtin names cannot "goto-definition".
        ResolveResult::Builtin(_) => return None,
    };

    Some(NavigationTarget {
        file_id,
        focus_range,
        full_range,
    })
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check(fixture: &str, expect: Expect) {
        let (db, file_id, [pos]) = TestDB::single_file(fixture).unwrap();
        let src = db.file_content(file_id);
        let got = match super::goto_definition(&db, file_id, pos) {
            None => String::new(),
            Some(target) => {
                assert!(target.full_range.contains_range(target.focus_range));
                let mut full = src[target.full_range].to_owned();
                let relative_focus = target.focus_range - target.full_range.start();
                full.insert(relative_focus.end().into(), '>');
                full.insert(relative_focus.start().into(), '<');
                full
            }
        };
        expect.assert_eq(&got);
    }

    #[test]
    fn not_found() {
        check("$0a", expect![]);
        check("b: $0a", expect![]);
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
        check("with a; $0b", expect!["<with> a;"]);
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
