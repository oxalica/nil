//! Remove empty `let in ...`.
//!
//! ```nix
//! let in { foo = "bar"; }
//! ```
//! =>
//! ```nix
//! { foo = "bar"; }
//! ```
use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use syntax::ast::{self, HasBindings};

pub(super) fn remove_empty_let_in(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::LetIn>()?;

    // There should be no bindings.
    if node.bindings().next().is_some() {
        return None;
    };

    // Remove trailing whitespace.
    let last_token = node
        .in_token()?
        .next_token()
        .filter(|tok| tok.kind().is_whitespace())
        .or(node.in_token())?;

    let range = node
        .let_token()?
        .text_range()
        .cover(last_token.text_range());

    ctx.add(
        "remove_empty_let_in",
        "Remove the empty `let-in`",
        AssistKind::QuickFix,
        vec![TextEdit {
            delete: range,
            insert: Default::default(),
        }],
    );

    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    define_check_assist!(super::remove_empty_let_in);

    #[test]
    fn simple() {
        check("$0let in { }", expect!["{ }"]);
        check("let $0 in { }", expect!["{ }"]);
        check("let in$0 { }", expect!["{ }"]);
        check("{ foo = let $0 in 42; }", expect!["{ foo = 42; }"]);
        check_no("let foo = 42;$0 in foo");
        check_no("{ foo = let bar = 42;$0 in bar; }");
    }
}
