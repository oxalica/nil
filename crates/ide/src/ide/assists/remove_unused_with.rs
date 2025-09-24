//! Remove an unused `with ...`.
//!
//! ```nix
//! foo = with pkgs; [ pkgs.bar ];
//! ```
//! =>
//! ```nix
//! foo = [ pkgs.bar ];
//! ```
use super::{AssistKind, AssistsCtx};
use crate::DiagnosticKind::UnusedWith;
use crate::TextEdit;
use syntax::ast;

pub(super) fn remove_unused_with(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let cursor_with = ctx.covering_node::<ast::With>()?;
    let with_range = cursor_with.with_token()?.text_range();

    let file = ctx.frange.file_id;
    let check = ctx.db.liveness_check(file);
    let diags = check.as_ref().to_diagnostics(ctx.db, file);

    let no_relevant_diags = diags
        .filter(|d| d.kind == UnusedWith && d.range.intersect(with_range).is_some())
        .count()
        == 0;

    if no_relevant_diags {
        return None;
    }

    let semicolon_token = cursor_with.semicolon_token()?;
    let trivia_range = std::iter::successors(semicolon_token.next_token(), |tok| tok.next_token())
        .take_while(|tok| tok.kind().is_trivia())
        .last()
        .unwrap_or(semicolon_token);

    ctx.add(
        "remove_unused_with",
        "Remove unused with",
        AssistKind::QuickFix,
        vec![TextEdit {
            delete: with_range.cover(trivia_range.text_range()),
            insert: Default::default(),
        }],
    );

    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    define_check_assist!(super::remove_unused_with);

    #[test]
    fn in_use_with() {
        check_no("a: $0with lib; bar");
    }

    #[test]
    fn unused_with() {
        // Simple
        check("a: $0with 1;a", expect!["a: a"]);

        // With trivia
        check("a: $0with /* trivia */ 1;   a", expect!["a: a"]);

        // With array
        check("a: $0with [ 1 ];   a", expect!["a: a"]);

        // 1st of multiple
        check("a: $0with 1;     with 2; a", expect!["a: with 2; a"]);

        // 2nd of multiple
        check("a: with 1; $0with 2;     a", expect!["a: with 1; a"]);
    }
}
