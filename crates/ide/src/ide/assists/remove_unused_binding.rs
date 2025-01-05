//! Remove an unused pattern binding.
//!
//! ```nix
//! { foo, bar }:{ foo = 1; }
//! ```
//! =>
//! ```nix
//! { foo }:{ foo = 1; }
//! ```
//!
//! Or, remove an unused let binding.
//!
//! ```nix
//! let
//! foo.bar = 1;
//! foo.baz = 2;
//! in {}
//! ```
//! =>
//! ```nix
//! let
//! in {}
//! ```
use super::{AssistKind, AssistsCtx};
use crate::def::AstPtr;
use crate::DiagnosticKind::UnusedBinding;
use crate::TextEdit;
use syntax::ast::AstNode;
use syntax::rowan::SyntaxToken;
use syntax::{ast, NixLanguage, SyntaxKind, SyntaxNode, TextRange};

pub(super) fn remove_unused_binding(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let cursor_attr = ctx.covering_node::<ast::Attr>()?;
    let syntax = cursor_attr.syntax();
    let range = syntax.text_range();

    let file = ctx.frange.file_id;
    let check = ctx.db.liveness_check(file);
    let diags = check.as_ref().to_diagnostics(ctx.db, file);

    let no_relevant_diags = diags
        .filter(|d| d.kind == UnusedBinding && d.range.intersect(range).is_some())
        .count()
        == 0;

    if no_relevant_diags {
        return None;
    }

    let edits = delete_let_binding(ctx).or_else(|| delete_pattern_binding(syntax))?;
    ctx.add(
        "remove_unused_binding",
        "Remove unused binding",
        AssistKind::QuickFix,
        edits,
    );

    Some(())
}

fn delete_let_binding(ctx: &mut AssistsCtx) -> Option<Vec<TextEdit>> {
    // Only match Attr in `attr.path = value;`.
    let cursor_attr = ctx.covering_node::<ast::Attr>()?;

    let file = ctx.frange.file_id;
    let src = ctx.db.file_content(file);
    let source_map = ctx.db.source_map(file);
    let name = source_map.name_for_node(AstPtr::new(cursor_attr.syntax()))?;

    // Collect all bindings of this Attr.
    let mut bindings = String::new();
    let mut edits = Vec::new();
    for ptr in source_map.nodes_for_name(name) {
        let attr = ast::Attr::cast(ptr.to_node(ctx.ast.syntax()))?;
        let path = ast::Attrpath::cast(attr.syntax().parent()?)?;
        let path_value = ast::AttrpathValue::cast(path.syntax().parent()?)?;

        let mut path_attr = path.attrs();

        let binding_range = path_attr.next()?.syntax().text_range();

        // Collect comments and whitespaces before each binding.
        let trivia_start =
            std::iter::successors(path_value.syntax().first_token(), |tok| tok.prev_token())
                .skip(1)
                .take_while(|tok| tok.kind().is_trivia())
                .last()
                .map_or_else(
                    || path_value.syntax().text_range().start(),
                    |tok| tok.text_range().start(),
                );
        let trivia_range = TextRange::new(trivia_start, path_value.syntax().text_range().start());

        bindings += &src[trivia_range];
        bindings += &src[binding_range];

        // Delete the binding.
        edits.push(TextEdit {
            delete: path_value.syntax().text_range().cover_offset(trivia_start),
            insert: Default::default(),
        });
    }

    Some(edits)
}

fn delete_pattern_binding(syntax: &SyntaxNode) -> Option<Vec<TextEdit>> {
    let mut delete_range = syntax.text_range();
    let mut cur_token = syntax.last_token()?;

    // Determine if this is the last pattern in the list
    cur_token = skip_trivia(
        std::iter::successors(syntax.last_token(), |tok| tok.next_token()).skip(1),
        cur_token,
    );
    cur_token = cur_token.next_token()?;
    let is_last = cur_token.kind() != SyntaxKind::COMMA;

    if is_last {
        // If the pattern is last in list, then go back and remove the preceding whitespace and comma
        cur_token = skip_trivia(
            std::iter::successors(syntax.last_token(), |tok| tok.prev_token()).skip(1),
            cur_token,
        );
        cur_token = cur_token.prev_token()?;
        if cur_token.kind() == SyntaxKind::COMMA {
            cur_token = skip_trivia(
                std::iter::successors(cur_token.prev_token(), |tok| tok.prev_token()),
                cur_token,
            );
            delete_range = delete_range.cover(cur_token.text_range());
        }
    } else {
        // If the pattern in not last in list, then continue to delete the following whitespace and comma
        cur_token = skip_trivia(
            std::iter::successors(cur_token.next_token(), |tok| tok.next_token()),
            cur_token,
        );
        delete_range = delete_range.cover(cur_token.text_range());
    }

    Some(vec![TextEdit {
        delete: delete_range,
        insert: Default::default(),
    }])
}

fn skip_trivia<I>(successors: I, default: SyntaxToken<NixLanguage>) -> SyntaxToken<NixLanguage>
where
    I: Iterator<Item = SyntaxToken<NixLanguage>>,
{
    successors
        .take_while(|tok| tok.kind().is_trivia())
        .last()
        .unwrap_or(default)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    define_check_assist!(super::remove_unused_binding);

    #[test]
    fn in_use_pattern_binding() {
        check_no("{ $0stdenv }: stdenv.mkDerivation { } ");
    }

    #[test]
    fn in_use_let_binding() {
        check_no(
            "
let
  $0foo.bar = 1;
  $1foo.baz = 2;
in foo
",
        );
    }

    #[test]
    fn unused_pattern_binding() {
        // First
        check(
            "{ $0hello , stdenv }: stdenv.mkDerivation { }",
            expect![[r#"{ stdenv }: stdenv.mkDerivation { }"#]],
        );

        // With trivia
        check(
            "{ $0hello  /* trivia */, stdenv }: stdenv.mkDerivation { } ",
            expect![[r#"{ stdenv }: stdenv.mkDerivation { }"#]],
        );

        // Last
        check(
            "{ stdenv , $0hello }: stdenv.mkDerivation { }",
            expect![[r#"{ stdenv }: stdenv.mkDerivation { }"#]],
        );

        // Single
        check(
            " { $0hello }: stdenv.mkDerivation { } ",
            expect![[r#" {  }: stdenv.mkDerivation { }"#]],
        );
    }

    #[test]
    fn unused_let_binding() {
        // Flat
        check(
            "
let
  $0foo.bar = 1;
  foo.baz = 2;
in
",
            expect![[r#"
let
in
"#]],
        );

        // With trivia
        check(
            "
let
  /* before */
  $0foo.bar = 1;
  /* middle */
  foo.baz = 2;
  /* after */
in
",
            expect![[r#"
let
  /* after */
in
"#]],
        );

        // Nested
        check(
            "
let
  $0foo = {
    bar = 1;
    baz = 2;
  };
in
",
            expect![[r#"
let
in
"#]],
        );

        // Flat & nested
        check(
            "
let
  $0foo = {
    bar = 1;
  };
  foo.baz = 2;
in
",
            expect![[r#"
let
in
"#]],
        );

        // Used with unused
        check(
            "
let
  $0foo = {
    bar = 1;
  };
  bar = 2;
in bar
",
            expect![[r#"
let
  bar = 2;
in bar
"#]],
        );

        // lambda
        check(
            "
let
  $0x = a: { b }: c@{}: 0;
in
",
            expect![[r#"
let
in
"#]],
        );
    }
}
