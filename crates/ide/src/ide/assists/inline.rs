use super::{AssistKind, AssistsCtx};
use crate::def::{AstPtr, Name, ResolveResult};
use crate::TextEdit;
use la_arena::Idx;
use smol_str::{SmolStr, ToSmolStr};
use syntax::ast::{AstNode, Expr};
use syntax::{ast, SyntaxNode, TextRange};

pub(super) fn inline(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let file_id = ctx.frange.file_id;
    let name_res = ctx.db.name_resolution(file_id);
    let source_map = ctx.db.source_map(file_id);

    let definition_of = |name: Idx<Name>| -> Option<(Expr, ast::AttrpathValue)> {
        let nodes = source_map.nodes_for_name(name).collect::<Vec<_>>();
        // Only provide assist when there is only one node
        // i.e. `let a.b = 1; a.c = 2; in a` is not supported
        if let [ptr] = nodes.as_slice() {
            ptr.to_node(ctx.ast.syntax())
                .parent()?
                .parent()
                .and_then(|node| {
                    let path_value = ast::AttrpathValue::cast(node)?;
                    Some((path_value.value()?, path_value))
                })
        } else {
            None
        }
    };

    let mut rewrites: Vec<TextEdit> = vec![];
    if let Some(usage) = ctx.covering_node::<ast::Ref>() {
        let ptr = AstPtr::new(usage.syntax());
        let expr = source_map.expr_for_node(ptr)?;
        let &ResolveResult::Definition(name) = name_res.get(expr)? else {
            return None;
        };
        let (definition, _) = definition_of(name)?;
        replace_usage(&mut rewrites, &definition, usage.syntax());
    } else if let Some(attr) = ctx.covering_node::<ast::Attr>() {
        let attr_syntax = attr.syntax();

        // `foo` in `{ foo }: …` is considered as an attr.
        // PatField is a bind site without definition so doesn't make sense here.
        if attr_syntax.parent().and_then(ast::PatField::cast).is_some() {
            return None;
        }

        // `foo` in { inherit `foo`; } is considered as an attr.
        if attr_syntax.parent().and_then(ast::Inherit::cast).is_some()
            && attr_syntax
                .parent()?
                .parent()
                .and_then(ast::LetIn::cast)
                .is_none()
        {
            // Attr is an usage here
            let ptr = AstPtr::new(attr.syntax());
            let expr_id = source_map.expr_for_node(ptr)?;
            let &ResolveResult::Definition(name) = name_res.get(expr_id)? else {
                return None;
            };
            let (definition, _) = definition_of(name)?;
            replace_usage(&mut rewrites, &definition, attr.syntax());
        } else {
            // attr is a definition here
            let ptr = AstPtr::new(attr.syntax());
            let name = source_map.name_for_node(ptr)?;

            let (definition, path_value) = definition_of(name)?;
            // Don't provide assist when there are more than one attrname
            if path_value.attrpath()?.attrs().count() > 1 {
                return None;
            };

            let usages = name_res.iter().filter_map(|(id, res)| match res {
                &ResolveResult::Definition(def) if def == name => source_map
                    .node_for_expr(id)
                    .map(|ptr| ptr.to_node(ctx.ast.syntax())),
                _ => None,
            });

            let is_letin = ast::LetIn::cast(path_value.syntax().parent()?).is_some();
            if is_letin {
                rewrites.push(TextEdit {
                    delete: path_value.syntax().text_range(),
                    insert: Default::default(),
                });
            };

            for usage in usages {
                replace_usage(&mut rewrites, &definition, &usage);
            }
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

// Replace an usage of a binding
fn replace_usage(rewrites: &mut Vec<TextEdit>, replacement: &ast::Expr, original: &SyntaxNode) {
    if let Some(inherit) = original.parent().and_then(ast::Inherit::cast) {
        // Delete one inherit field
        rewrites.push(TextEdit {
            delete: original.text_range(),
            insert: Default::default(),
        });

        // Insert new binding right after
        rewrites.push(TextEdit {
            delete: TextRange::new(
                inherit.syntax().text_range().end(),
                inherit.syntax().text_range().end(),
            ),
            // Unambiguous and never need parens
            insert: format!(" {} = {};", original.text(), replacement.syntax().text()).into(),
        });
    } else {
        rewrites.push(TextEdit {
            delete: original.text_range(),
            insert: maybe_parenthesize(replacement, original),
        });
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    define_check_assist!(super::inline);

    #[test]
    fn let_in_ref() {
        check("let a = 1; in $0a", expect!["let a = 1; in 1"]);
        check(
            "let a = x: x; in $0a 1",
            expect!["let a = x: x; in (x: x) 1"],
        );
    }

    #[test]
    fn let_in_def() {
        check("let $0a = x: x; in a a", expect!["let  in (x: x) (x: x)"]);
    }

    #[test]
    fn no_let_in_multi() {
        check_no("let a.b = 1; a.c = 2; in $0a");
        check_no("let a.b$0 = 1; a.c = 2; in a");
        check_no("let $0a.b = 1; a.c = 2; in a");
    }

    #[test]
    fn rec_attr_ref() {
        check(
            "rec { foo = 1; bar = $0foo; }",
            expect!["rec { foo = 1; bar = 1; }"],
        );
    }

    #[test]
    fn rec_attr_def() {
        check(
            "rec { $0foo = 1; bar = foo; baz = foo; }",
            expect!["rec { foo = 1; bar = 1; baz = 1; }"],
        );
    }

    #[test]
    fn allow_inherit_usage_def() {
        check(
            "let $0foo = 1; in { inherit foo; }",
            expect!["let  in { inherit ; foo = 1; }"],
        );
        check(
            "let $0foo = 1; bar = 2; in { inherit foo bar; }",
            expect!["let  bar = 2; in { inherit  bar; foo = 1; }"],
        );
    }

    #[test]
    fn allow_inherit_usage_ref() {
        check(
            "let foo = 1; in { inherit $0foo; }",
            expect!["let foo = 1; in { inherit ; foo = 1; }"],
        );
        check(
            "let foo = 1; bar = 2; in { inherit $0foo bar; }",
            expect!["let foo = 1; bar = 2; in { inherit  bar; foo = 1; }"],
        );
    }

    #[test]
    fn no_allow_inherit_as_def() {
        check_no("let inherit (lib) $0foo; in foo");
        check_no("let inherit (lib) foo; in $0foo");
    }

    #[test]
    fn no_patfield() {
        check_no("{ outputs = { nixpkgs, ... }: { inherit $0nixpkgs; }; }");
        check_no("{ outputs = { $0nixpkgs, ... }: { inherit nixpkgs; }; }");
        check_no("{ outputs = { $0nixpkgs, ... }: nixpkgs; }");
    }
}
