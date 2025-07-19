use super::{AssistKind, AssistsCtx};
use crate::def::{AstPtr, BindingValue, Expr};
use crate::TextEdit;
use syntax::ast::AstNode;
use syntax::{ast, TextRange};

// Add unknown symbols to inherit clauses
pub(super) fn add_to_inherit(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let file_id = ctx.frange.file_id;
    let unbound = ctx.covering_node::<ast::Ref>()?;
    let name_res = ctx.db.name_resolution(file_id);
    let source_map = ctx.db.source_map(file_id);

    let ptr = AstPtr::new(unbound.syntax());
    let expr_id = source_map.expr_for_node(ptr)?;

    // The reference is defined, do nothing
    if name_res.get(expr_id).is_some() {
        return None;
    }

    let module = ctx.db.module(file_id);
    let mut stack = vec![module.entry_expr()];
    while let Some(expr) = stack.pop() {
        match &module[expr] {
            Expr::LetIn(bindings, body) => {
                if let Some((bindname, bindvalue)) =
                    // Due to the multiplicity of inherit clauses (i.e. `let inherit (lib) foo bar baz; in ...`),
                    // we only care about the last one where we are going to append the unbound expression.
                    bindings.statics.last()
                {
                    let bind_nodes = source_map
                        .nodes_for_name(*bindname)
                        .map(|ptr| ptr.to_node(ctx.ast.syntax()))
                        .collect::<Vec<_>>();
                    let last_bind = bind_nodes.last()?;

                    let unbound_text = unbound.syntax().text();
                    let label = match bindvalue {
                        BindingValue::Inherit(_) => format!("Inherit \"{unbound_text}\""),
                        BindingValue::InheritFrom(idx) => {
                            let from_clause = source_map
                                .node_for_expr(bindings.inherit_froms[*idx])?
                                .to_node(ctx.ast.syntax())
                                .text()
                                .to_string();

                            format!("Inherit \"{unbound_text}\" from {from_clause}")
                        }
                        // We only generate assist for inherits
                        _ => continue,
                    };

                    ctx.add(
                        "add_to_inherit",
                        label,
                        AssistKind::RefactorRewrite,
                        vec![TextEdit {
                            delete: TextRange::new(
                                last_bind.text_range().end(),
                                last_bind.text_range().end(),
                            ),
                            insert: format!(" {}", unbound_text).into(),
                        }],
                    );
                }
                stack.push(*body);
            }
            e => e.walk_child_exprs(|e| stack.push(e)),
        }
    }

    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    define_check_assist!(super::add_to_inherit);

    #[test]
    fn simple() {
        check(
            "let inherit a; in $0foo",
            expect!["let inherit a foo; in foo"],
        );
        check(
            "let inherit (lib) a; in $0foo",
            expect!["let inherit (lib) a foo; in foo"],
        );
        check(
            "let inherit (lib.types) a; in $0foo",
            expect!["let inherit (lib.types) a foo; in foo"],
        );
    }
}
