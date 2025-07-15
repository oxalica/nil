use super::{AssistKind, AssistsCtx};
use crate::def::{AstPtr, ResolveResult};
use crate::TextEdit;
use smol_str::ToSmolStr;
use syntax::ast::AstNode;
use syntax::{ast, match_ast};

pub(super) fn inline(ctx: &mut AssistsCtx<'_>) -> Option<()> {
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
    let replacement_text = {
        let replacement_node = replacement.syntax();
        let parent = covering_node.syntax().parent().and_then(ast::Expr::cast);

        let need_paren =
            matches!(parent, Some(outer) if !outer.contains_without_paren(&replacement));

        if need_paren {
            format!("({})", replacement_node.text()).to_smolstr()
        } else {
            replacement_node.to_smolstr()
        }
    };

    ctx.add(
        "inline",
        format!("Inline binding `{}`", covering_token.text()),
        AssistKind::RefactorInline,
        vec![TextEdit {
            delete: covering_token.text_range(),
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
    }

    #[test]
    fn attr() {
        check(
            "rec { foo = 1; bar = $0foo; }",
            expect!["rec { foo = 1; bar = 1; }"],
        );
    }
}
