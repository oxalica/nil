use super::{AssistKind, AssistsCtx};
use crate::def::{AstPtr, ResolveResult};
use crate::TextEdit;
use smol_str::{SmolStr, ToSmolStr};
use syntax::ast::AstNode;
use syntax::{ast, SyntaxNode};

pub(super) fn inline(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let file_id = ctx.frange.file_id;
    let parse = ctx.db.parse(file_id);
    let name_res = ctx.db.name_resolution(file_id);
    let source_map = ctx.db.source_map(file_id);

    let mut rewrites: Vec<TextEdit> = vec![];

    if let Some(usage) = ctx.covering_node::<ast::Ref>() {
        let ptr = AstPtr::new(usage.syntax());
        let expr_id = source_map.expr_for_node(ptr)?;
        let &ResolveResult::Definition(name) = name_res.get(expr_id)? else {
            return None;
        };
        let definition = source_map.nodes_for_name(name).find_map(|ptr| {
            ptr.to_node(&parse.syntax_node())
                .ancestors()
                .flat_map(ast::AttrpathValue::cast)
                .find_map(|path_value| path_value.value())
        })?;

        rewrites.push(TextEdit {
            delete: usage.syntax().text_range(),
            insert: maybe_parenthesize(&definition, usage.syntax()),
        });
    } else if let Some(definition) = ctx.covering_node::<ast::Attr>() {
        let ptr = AstPtr::new(definition.syntax());
        let name_id = source_map.name_for_node(ptr)?;
        let path_value = definition
            .syntax()
            .ancestors()
            .find_map(ast::AttrpathValue::cast)?;
        let definition = path_value.value()?;

        let usages = name_res
            .iter()
            .filter_map(|(id, res)| match res {
                &ResolveResult::Definition(def) if def == name_id => source_map
                    .node_for_expr(id)
                    .map(|ptr| ptr.to_node(ctx.ast.syntax())),
                _ => None,
            })
            .collect::<Vec<_>>();

        let is_letin = ast::LetIn::cast(path_value.syntax().parent()?).is_some();
        if is_letin {
            rewrites.push(TextEdit {
                delete: path_value.syntax().text_range(),
                insert: Default::default(),
            });
        };

        for usage in usages {
            rewrites.push(TextEdit {
                delete: usage.text_range(),
                insert: maybe_parenthesize(&definition, &usage),
            });
        }
    } else {
        return None;
    };

    ctx.add(
        "inline",
        "Inline binding",
        AssistKind::RefactorInline,
        rewrites,
    );

    Some(())
}

// Parenthesize a node properly given the replacement context
fn maybe_parenthesize(replacement: &ast::Expr, original: &SyntaxNode) -> SmolStr {
    let parent = original.parent().and_then(ast::Expr::cast);
    let need_paren = matches!(parent, Some(outer) if !outer.contains_without_paren(replacement));
    if need_paren {
        format!("({})", replacement.syntax()).to_smolstr()
    } else {
        replacement.syntax().to_smolstr()
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    define_check_assist!(super::inline);

    #[test]
    fn let_in_ref() {
        check(
            r#"let a = "foo"; in $0a"#,
            expect![r#"let a = "foo"; in "foo""#],
        );
    }

    #[test]
    fn let_in_lambda_ref() {
        check(
            "let a = x: x; in $0a 1",
            expect!["let a = x: x; in (x: x) 1"],
        );
    }

    #[test]
    fn attr_ref() {
        check(
            "rec { foo = 1; bar = $0foo; }",
            expect!["rec { foo = 1; bar = 1; }"],
        );
    }

    #[test]
    fn let_in_def() {
        check("let $0a = x: x; in a a", expect!["let  in (x: x) (x: x)"]);
    }
}
