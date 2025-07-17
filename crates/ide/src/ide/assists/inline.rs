use super::{AssistKind, AssistsCtx};
use crate::def::{AstPtr, ResolveResult};
use crate::TextEdit;
use smol_str::{SmolStr, ToSmolStr};
use syntax::ast::AstNode;
use syntax::{ast, match_ast, SyntaxNode};

pub(super) fn inline_from_reference(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let file_id = ctx.frange.file_id;
    let parse = ctx.db.parse(file_id);

    let covering_node = ctx.covering_node::<ast::Ref>()?;
    let covering_token = covering_node.token()?;
    let ptr = AstPtr::new(covering_node.syntax());

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
    let replacement_text = maybe_parenthesize(&replacement, covering_node.syntax());

    ctx.add(
        "inline_from_reference",
        format!("Inline binding `{}`", covering_token.text()),
        AssistKind::RefactorInline,
        vec![TextEdit {
            delete: covering_token.text_range(),
            insert: replacement_text,
        }],
    );

    Some(())
}

pub(super) fn inline_from_definition(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let file_id = ctx.frange.file_id;
    let name_res = ctx.db.name_resolution(file_id);

    let covering_node = ctx.covering_node::<ast::Attr>()?;
    let source_map = ctx.db.source_map(file_id);
    let ptr = AstPtr::new(covering_node.syntax());
    let name_id = source_map.name_for_node(ptr)?;

    let binding_pathvalue = covering_node
        .syntax()
        .ancestors()
        .find_map(ast::AttrpathValue::cast)?;
    let binding_definition = binding_pathvalue.value()?;

    let binding_usages = name_res
        .iter()
        .filter_map(|(id, res)| match res {
            &ResolveResult::Definition(def) if def == name_id => source_map.node_for_expr(id),
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut rewrites: Vec<TextEdit> = vec![];
    let parent = binding_pathvalue.syntax().parent()?;
    let is_letin = ast::LetIn::cast(parent.clone()).is_some();
    if is_letin {
        rewrites.push(TextEdit {
            delete: binding_pathvalue.syntax().text_range(),
            insert: Default::default(),
        });
    };
    for usage in binding_usages {
        let usage_node = usage.to_node(ctx.ast.syntax());
        let replacement_text = maybe_parenthesize(&binding_definition, &usage_node);

        rewrites.push(TextEdit {
            delete: usage.text_range(),
            insert: replacement_text,
        });
    }

    ctx.add(
        "inline_from_definition",
        format!("Inline binding `{}`", covering_node.syntax().text()),
        AssistKind::RefactorRewrite,
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

    #[test]
    fn let_in_ref() {
        define_check_assist!(super::inline_from_reference);
        check(
            r#"let a = "foo"; in $0a"#,
            expect![r#"let a = "foo"; in "foo""#],
        );
    }

    #[test]
    fn let_in_lambda_ref() {
        define_check_assist!(super::inline_from_reference);
        check(
            "let a = x: x; in $0a 1",
            expect!["let a = x: x; in (x: x) 1"],
        );
    }

    #[test]
    fn attr_ref() {
        define_check_assist!(super::inline_from_reference);
        check(
            "rec { foo = 1; bar = $0foo; }",
            expect!["rec { foo = 1; bar = 1; }"],
        );
    }

    #[test]
    fn let_in_def() {
        define_check_assist!(super::inline_from_definition);
        check("let $0a = x: x; in a a", expect!["let  in (x: x) (x: x)"]);
    }
}
