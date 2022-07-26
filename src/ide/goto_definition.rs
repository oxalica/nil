use super::NavigationTarget;
use crate::def::{AstPtr, DefDatabase};
use crate::FileId;
use rowan::ast::AstNode;
use rowan::TextSize;
use syntax::{ast, SyntaxKind, T};

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
    let node = tok.parent_ancestors().find_map(ast::Ref::cast)?;

    let source_map = db.source_map(file_id);
    let expr_id = source_map.node_expr(AstPtr::new(node.syntax()))?;
    let def_id = db.resolve_name(file_id, expr_id)?;
    let def_node = source_map
        .name_def_node(def_id)?
        .to_node(&parse.syntax_node());
    let full_node = def_node.ancestors().find(|n| {
        matches!(
            n.kind(),
            SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
        )
    })?;

    Some(NavigationTarget {
        file_id,
        full_range: full_node.text_range(),
        focus_range: def_node.text_range(),
    })
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check(src: &str, expect: Expect) {
        let (db, file_id, pos) = TestDB::from_file_with_pos(src);
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
}
