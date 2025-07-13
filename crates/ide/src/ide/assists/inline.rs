use super::{AssistKind, AssistsCtx};
use crate::def::ResolveResult;
use crate::{def::AstPtr, TextEdit};
use smol_str::ToSmolStr;
use syntax::ast::{AstNode, Binding, HasBindings};
use syntax::match_ast;
use syntax::{ast, best_token_at_offset};

pub(super) fn inline(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let parse = ctx.db.parse(ctx.frange.file_id);
    let file_id = ctx.frange.file_id;

    let token = best_token_at_offset(&parse.syntax_node(), ctx.frange.range.start())?;
    let ptr = token.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => Some(AstPtr::new(n.syntax())),
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
                        ast::LetIn(let_in) => {
                            let_in.bindings().find_map(|binding| match binding {
                                Binding::AttrpathValue(path_value) => path_value.value(),
                                Binding::Inherit(_) => None,
                            })
                        },
                        _ => None,
                    }
                }
            })?;

            Some(definition_value)
        }),
        // TODO: ignore anything else for now
        _ => None,
    }?;

    ctx.add(
        "inline",
        "Inline expression",
        AssistKind::RefactorRewrite,
        vec![TextEdit {
            delete: token.text_range(),
            insert: AstNode::syntax(&replacement).to_smolstr(),
        }],
    );

    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    #[test]
    fn let_in() {
        define_check_assist!(super::inline);
        check(
            r#"let a = "foo"; in $0a"#,
            expect![r#"let a = "foo"; in "foo""#],
        );
    }
}
