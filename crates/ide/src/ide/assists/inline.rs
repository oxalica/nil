use super::{AssistKind, AssistsCtx};
use crate::def::ResolveResult;
use crate::{def::AstPtr, TextEdit};
use smol_str::ToSmolStr;
use syntax::ast::AstNode;
use syntax::match_ast;
use syntax::{ast, best_token_at_offset};

pub(super) fn inline(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let parse = ctx.db.parse(ctx.frange.file_id);
    let file_id = ctx.frange.file_id;

    let token = best_token_at_offset(&parse.syntax_node(), ctx.frange.range.start())?;
    let (parenthesized, ptr) = token.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => {
                    let is_in_paren = node.parent()
                        .map(|parent| ast::Paren::cast(parent).is_some())
                        .unwrap_or(false);

                    Some((is_in_paren, AstPtr::new(n.syntax())))
                },
                _ => None,
            }
        }
    })?;

    let name_res = ctx.db.name_resolution(file_id);
    let source_map = ctx.db.source_map(file_id);
    let expr_id = source_map.expr_for_node(ptr)?;

    let replacement = match name_res.get(expr_id)? {
        &ResolveResult::Definition(name) => source_map.nodes_for_name(name).find_map(|ptr| {
            let definition_node = ptr.to_node(&parse.syntax_node());
            let definition_value = definition_node.ancestors().find_map(|n| {
                match_ast! {
                    match n {
                        ast::AttrpathValue(path_value) => path_value.value(),
                        _ => None,
                    }
                }
            })?;

            Some(definition_value)
        }),
        // Only let-ins are rewritable
        _ => None,
    }?;
    let replacement_text = {
        let node = AstNode::syntax(&replacement);
        let do_parenthesize = match &replacement {
            ast::Expr::Lambda(_) => true,
            _ => false,
        };

        if do_parenthesize && !parenthesized {
            format!("({})", node.text()).to_smolstr()
        } else {
            node.to_smolstr()
        }
    };

    ctx.add(
        "inline",
        format!("Inline expression `{}`", token.text()),
        AssistKind::RefactorRewrite,
        vec![TextEdit {
            delete: token.text_range(),
            insert: replacement_text,
        }],
    );

    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    define_check_assist!(super::inline);

    #[test]
    fn let_in() {
        check(
            r#"let a = "foo"; in $0a"#,
            expect![r#"let a = "foo"; in "foo""#],
        );
    }

    #[test]
    fn let_in_lambda() {
        check(
            "let a = x: x; in $0a 1",
            expect!["let a = x: x; in (x: x) 1"],
        );

        // If for some reason the expression is already parenthesized,
        // do not duplicate it.
        check(
            "let a = x: x; in ($0a) 1",
            expect!["let a = x: x; in (x: x) 1"],
        );
    }

    #[test]
    fn attr() {
        check(
            "rec { foo = 1; bar = $0foo; }",
            expect!["rec { foo = 1; bar = 1; }"],
        );
    }
}
