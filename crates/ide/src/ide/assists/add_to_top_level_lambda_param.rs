//! Add an undefined name to the top-level lambda.
//!
//! ```nix
//! { foo }: foo + bar
//! ```
//! =>
//! ```nix
//! { foo, bar }: foo + bar
//! ```
use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use syntax::ast::{self, AstNode};
use syntax::{SyntaxNodePtr, TextRange, TextSize};

pub(super) fn add_to_top_level_lambda_param(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::Ref>()?;
    let name = node.token()?;
    let name = name.text();

    // The whole file should be a lambda.
    let pat = ast::Lambda::cast(ctx.ast.syntax().first_child()?)?
        .param()?
        .pat()?;

    // Name should be undefined.
    let expr = ctx
        .db
        .source_map(ctx.frange.file_id)
        .expr_for_node(SyntaxNodePtr::new(node.syntax()))?;
    if ctx
        .db
        .name_resolution(ctx.frange.file_id)
        .get(expr)
        .is_some()
    {
        return None;
    };

    let (pos, insert) = if let Some(field) = pat.fields().last() {
        let field = field.syntax();
        let mut pos = field.text_range().end();
        // Insert before the space if the field ends with a space.
        if matches!(field.last_token(), Some(tok) if tok.text().ends_with(' ')) {
            pos -= TextSize::from(1);
        }
        (pos, format!(", {name}"))
    } else if let Some(ellipsis) = pat.ellipsis_token() {
        (ellipsis.text_range().start(), format!("{name}, "))
    } else if let Some(curly) = pat.r_curly_token() {
        (curly.text_range().start(), format!("{name} "))
    } else {
        (pat.syntax().text_range().start(), name.into())
    };

    ctx.add(
        "add_to_top_level_lambda_param",
        format!("Add `{name}` to the top-level lambda parameter"),
        AssistKind::QuickFix,
        vec![TextEdit {
            delete: TextRange::new(pos, pos),
            insert: insert.into(),
        }],
    );

    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    define_check_assist!(super::add_to_top_level_lambda_param);

    #[test]
    fn simple() {
        check("{ }: foo$0", expect!["{ foo }: foo"]);
        check("{}: foo$0", expect!["{foo }: foo"]);
        check("{ foo }: b$0ar", expect!["{ foo, bar }: bar"]);
        check("{foo}: $0bar", expect!["{foo, bar}: bar"]);
        check("{ ... }: foo$0", expect!["{ foo, ... }: foo"]);
        check("{...}: foo$0", expect!["{foo, ...}: foo"]);
        check("{foo, ...}: $0bar", expect!["{foo, bar, ...}: bar"]);
        check("{foo,...}: $0bar", expect!["{foo, bar,...}: bar"]);

        check_no("{ foo }: fo$0o");
        check_no("{ }: foo.$0bar");
        check_no("{ }: let foo = bar; in f$0oo");
        check_no("{ bar }: { fo$0o = bar; }");
        check_no("bar: fo$0o");
    }
}
