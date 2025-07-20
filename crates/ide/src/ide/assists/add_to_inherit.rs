use super::{AssistKind, AssistsCtx};
use crate::def::AstPtr;
use crate::TextEdit;
use syntax::ast::{AstNode, Binding, HasBindings};
use syntax::{ast, TextRange};

// Add unknown symbols to inherit clauses
pub(super) fn add_to_inherit(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let file_id = ctx.frange.file_id;
    let unbound = ctx.covering_node::<ast::Ref>()?;
    let unbound_text = unbound.syntax().text();
    let name_res = ctx.db.name_resolution(file_id);
    let source_map = ctx.db.source_map(file_id);

    let ptr = AstPtr::new(unbound.syntax());
    let expr_id = source_map.expr_for_node(ptr)?;

    // The reference is defined, do nothing
    if name_res.get(expr_id).is_some() {
        return None;
    }

    // We walk upwards from the current node, adding suggestion for each parent let in that has an
    // inherit construct.
    for node in unbound.syntax().ancestors() {
        if let Some(let_in) = ast::LetIn::cast(node) {
            let inherits = let_in
                .bindings()
                .filter_map(|binding| match binding {
                    Binding::Inherit(x) => Some(x),
                    _ => None,
                })
                .collect::<Vec<_>>();

            // Start from the last one (closest one) to have a consistent order
            for inherit in inherits.iter().rev() {
                if let Some(inherit_from) = inherit.from_expr() {
                    let add_loc = inherit
                        .attrs()
                        .last()
                        .map(|last| last.syntax().text_range())
                        .unwrap_or(inherit_from.syntax().text_range());

                    ctx.add(
                        "add_to_inherit",
                        format!(
                            "Inherit \"{}\" from {}",
                            unbound_text,
                            inherit_from.syntax().text()
                        ),
                        AssistKind::RefactorRewrite,
                        vec![TextEdit {
                            delete: TextRange::new(add_loc.end(), add_loc.end()),
                            insert: format!(" {unbound_text}").into(),
                        }],
                    );
                }
            }
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
        // We go from closest to furthest, the first edit would be the farthest
        check(
            "let inherit (lib) a; inherit (lib.types) b; in $0foo",
            expect!["let inherit (lib) a foo; inherit (lib.types) b; in foo"],
        );
        check(
            "let inherit (lib.types) a; in $0foo",
            expect!["let inherit (lib.types) a foo; in foo"],
        );
    }
}
