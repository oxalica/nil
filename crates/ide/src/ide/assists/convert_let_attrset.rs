//! Convert `let {...}` to `let ... in ...`.
//!
//! ```nix
//! let { foo = "bar"; body = foo; }
//! ```
//! =>
//! ```nix
//! let foo = "bar" in foo
//! ```
use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use syntax::ast::{self, AstNode};

pub(super) fn convert_let_attrset(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::AttrSet>()?;

    if node.let_token().is_none() {
        return None;
    }

    let is_body_node = |child: &syntax::SyntaxNode| {
        if let Some(tok) = child.first_token() {
            tok.text() == "body"
        } else {
            false
        }
    };

    let body_binding = node.syntax().children().find(is_body_node)?.last_child()?;

    let bindings: String = node
        .syntax()
        .children()
        .filter(|child| !is_body_node(child))
        .map(|b| b.text().to_string())
        .collect::<Vec<String>>()
        .join(" ");

    ctx.add(
        "convert_let_attrset",
        "Convert `let {...}` to `let ... in ...`",
        AssistKind::QuickFix,
        vec![TextEdit {
            delete: node.syntax().text_range(),
            insert: format!("let {bindings} in {body_binding}").into(),
        }],
    );

    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    define_check_assist!(super::convert_let_attrset);

    #[test]
    fn simple() {
        check(
            r#"let { $0foo = "bar"; body = foo; }"#,
            expect![r#"let foo = "bar"; in foo"#],
        );
        check_no(r#"{ $0foo = "bar"; }"#);
    }

    #[test]
    fn multiple_bindings() {
        check(
            r#"let { $0foo = "foo"; bar = "bar"; body = foo + bar; }"#,
            expect![r#"let foo = "foo"; bar = "bar"; in foo + bar"#],
        )
    }
}
