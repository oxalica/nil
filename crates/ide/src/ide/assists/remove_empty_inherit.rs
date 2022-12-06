//! Remove an empty `inherit`.
//!
//! ```nix
//! { foo = "bar"; inherit; }
//! ```
//! =>
//! ```nix
//! { foo = "bar"; }
//! ```
use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use syntax::ast::{self, AstNode};
use syntax::SyntaxKind;

pub(super) fn remove_empty_inherit(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::Inherit>()?;

    // There should be no attrs inherited.
    if node.attrs().next().is_some() {
        return None;
    }

    let syntax = node.syntax();
    let mut range = syntax.text_range();
    // Also remove trailing SPACEs.
    if let Some(ws) = syntax
        .last_token()?
        .next_token()
        .filter(|tok| tok.kind() == SyntaxKind::SPACE)
    {
        range = range.cover(ws.text_range());
    }

    ctx.add(
        "remove_empty_inherit",
        "Remove the empty `inherit`",
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

    define_check_assist!(super::remove_empty_inherit);

    #[test]
    fn simple() {
        check("{ inherit;$0 }", expect!["{ }"]);
        check("{ $0inherit ({ }); }", expect!["{ }"]);
        check(
            r#"{ foo = "bar"; inherit$0; }"#,
            expect![r#"{ foo = "bar"; }"#],
        );
        check(r#"let inhe$0rit; in "foo""#, expect![r#"let in "foo""#]);
        check("let inherit ({ $0foo = 42; }); in 42", expect!["let in 42"]);

        check("{inhe$0rit;}", expect!["{}"]);
        check("{inherit$0;\n}", expect!["{}"]);

        check_no("{ in$0herit foo; }");
        check_no("{ inher$0it ({ foo = 42; }) foo; }");
        check_no(r#"let inherit foo;$0 in foo"#);
        check_no(r#"let inherit ({ foo =$0 "bar"; }) foo; in foo"#);
    }
}
