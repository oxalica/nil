//! Pack one or more implicit bindings of implicit Attrset into explicit nested one.
//! FIXME: Indentations are not reformatted well.
//!
//! ```nix
//! {
//!   foo.bar = 1;
//!   foo.baz = 2;
//! }
//! ```
//! =>
//! ```nix
//! {
//!   foo = {
//!     bar = 1;
//!     baz = 2;
//!   };
//! }
//! ```
use super::{AssistKind, AssistsCtx};
use crate::def::AstPtr;
use crate::TextEdit;
use syntax::ast::{self, AstNode};
use syntax::TextRange;

pub(super) fn pack_bindings(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    // Only match Attr in `attr.path = value;`.
    let cursor_attr = ctx.covering_node::<ast::Attr>()?;
    // Sanity check.
    let cursor_path = ast::Attrpath::cast(cursor_attr.syntax().parent()?)?;
    let cursor_path_value = ast::AttrpathValue::cast(cursor_path.syntax().parent()?)?;

    let file = ctx.frange.file_id;
    let src = ctx.db.file_content(file);
    let source_map = ctx.db.source_map(file);
    let name = source_map.name_for_node(AstPtr::new(cursor_attr.syntax()))?;

    // Ignore unique final binding `foo.|bar = ..;`, but still enable the
    // conversion `foo.|bar.baz = ..` => `foo.bar = { baz = .. };`.
    if source_map.nodes_for_name(name).count() == 1
        && cursor_path.attrs().last().as_ref() == Some(&cursor_attr)
    {
        return None;
    }

    // FIXME: Should save RHS in Name.
    let is_rec = {
        let first_ptr = source_map.nodes_for_name(name).next()?;
        let path_value =
            ast::AttrpathValue::cast(first_ptr.to_node(ctx.ast.syntax()).parent()?.parent()?)?;
        matches!(path_value.value()?.flatten_paren()?, ast::Expr::AttrSet(set) if set.rec_token().is_some())
    };

    // Collect all inner bindings of this Attr.
    let mut inner_bindings = String::new();
    let mut edits = Vec::new();
    let mut cursor_edit_idx = None;
    for ptr in source_map.nodes_for_name(name) {
        let attr = ast::Attr::cast(ptr.to_node(ctx.ast.syntax()))?;
        let path = ast::Attrpath::cast(attr.syntax().parent()?)?;
        let path_value = ast::AttrpathValue::cast(path.syntax().parent()?)?;

        let mut iter = path.attrs().skip_while(|x| x.syntax() != attr.syntax());
        iter.next()?; // `attr`.
        let binding_range = match iter.next() {
            //          /-------------\ bindings
            // `foo.bar.baz.qux = expr;`
            //      ^^^ ^^^ inner_attr
            //      \ cursor_attr
            Some(inner_attr) => {
                let start = inner_attr.syntax().text_range().start();
                let end = path_value.semicolon_token()?.text_range().end();
                TextRange::new(start, end)
            }
            //             /---\ bindings
            // `foo.bar = { ... };`
            //      ^^^ cursor_attr
            None => match path_value.value()?.flatten_paren()? {
                ast::Expr::AttrSet(set) if set.let_token().is_none() => {
                    // Remove the delimiters while keep all comments inside.
                    let start = set.l_curly_token()?.text_range().end();
                    let end = set.r_curly_token()?.text_range().start();
                    TextRange::new(start, end)
                }
                // The inner value must be an Attrset. Or there should be merge failures.
                _ => return None,
            },
        };

        // Collect comments and whitespaces before each bindings.
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

        inner_bindings += &src[trivia_range];
        inner_bindings += &src[binding_range];

        // Delete the binding.
        // We will inject the result back to the cursor bindings later.
        edits.push(TextEdit {
            delete: path_value.syntax().text_range().cover_offset(trivia_start),
            insert: "".into(),
        });

        if attr.syntax() == cursor_attr.syntax() {
            cursor_edit_idx = Some(edits.len() - 1);
        }
    }

    // Replace the result into the cursor binding.
    let prefix_path_range = TextRange::new(
        cursor_path_value.syntax().text_range().start(),
        cursor_attr.syntax().text_range().end(),
    );
    edits[cursor_edit_idx?].insert = format!(
        "\n{} = {}{{{}\n}};",
        &src[prefix_path_range],
        if is_rec { "rec " } else { "" },
        inner_bindings
    )
    .into();

    ctx.add(
        "pack_bindings",
        format!("Pack into nested `{} = {{ .. }}`", cursor_attr.syntax()),
        AssistKind::RefactorRewrite,
        edits,
    );
    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    define_check_assist!(super::pack_bindings);

    #[test]
    fn no_final() {
        check_no("{ $0foo = 42; }");
        check_no("{ foo.$0bar = 42; }");
        check_no("{ $0foo = { bar = 1; baz = 1; } }");
    }

    #[test]
    fn single() {
        check(
            "{ ${1}.$0foo.bar = 1; ${1}.foo.qux = 2; }",
            expect![[r#"
            {
            ${1}.foo = { bar = 1;
            }; ${1}.foo.qux = 2; }
        "#]],
        );
        check(
            "{ services.asdf$0.enable = true; }",
            expect![[r#"
            {
            services.asdf = { enable = true;
            }; }
        "#]],
        );
    }

    #[test]
    fn no_duplicated_values() {
        check_no("{ $0foo = 1; foo = 2; }");
    }

    #[test]
    fn dynamic() {
        check_no("{ $0${1}.foo = 1; ${1}.bar = 2; }");
    }

    #[test]
    fn no_let_attrset() {
        check_no("{ $0foo = let { bar = 1; }; foo = { baz = 1; }; }");
    }

    #[test]
    fn pack_path_values() {
        check(
            "
{
  a = 1;
  $0foo.bar = 1;
  b = 2;
  foo.baz.bux = 2;
  c = 3;
}",
            expect![[r#"
                {
                  a = 1;
                foo = {
                  bar = 1;
                  baz.bux = 2;
                };
                  b = 2;
                  c = 3;
                }
            "#]],
        );
        check(
            "
{
  a = 1;
  foo.bar = 1;
  b = 2;
  $0foo.baz.bux = 2;
  c = 3;
}",
            expect![[r#"
                {
                  a = 1;
                  b = 2;
                foo = {
                  bar = 1;
                  baz.bux = 2;
                };
                  c = 3;
                }
            "#]],
        );
    }

    #[test]
    fn pack_sets() {
        check(
            "
{
  a = 1;
  foo.bar = 1;
  b = 2;
  $0foo = {
    a = 42;
    b = 24;
  };
  c = 3;
  foo = rec {
    d = 99;
  };
}",
            expect![[r#"
                {
                  a = 1;
                  b = 2;
                foo = {
                  bar = 1;

                    a = 42;
                    b = 24;


                    d = 99;

                };
                  c = 3;
                }
            "#]],
        );
    }

    #[test]
    fn keep_rec() {
        // Not rec.
        check(
            "
let
  $0foo.bar = 1;
  foo.baz = 2;
in foo
",
            expect![[r#"
                let
                foo = {
                  bar = 1;
                  baz = 2;
                };
                in foo
            "#]],
        );

        // Not rec.
        check(
            "
rec {
  $0foo = {
    bar = 1;
  };
  foo.baz = 2;
}
",
            expect![[r#"
                rec {
                foo = {

                    bar = 1;

                  baz = 2;
                };
                }
            "#]],
        );

        // Rec.
        check(
            "
{
  $0foo = rec {
    bar = 1;
  };
  foo.baz = 2;
}
",
            expect![[r#"
                {
                foo = rec {

                    bar = 1;

                  baz = 2;
                };
                }
            "#]],
        );

        // Rec.
        check(
            "
{
  foo = rec { };
  $0foo = { a = 42; };
}
",
            expect![[r#"
                {
                foo = rec {

                   a = 42;
                };
                }
            "#]],
        );
    }

    #[test]
    fn keep_comments() {
        check(
            "
{
  # aaa
  foo.a = 1;
  # bbb
  $0foo.b = 2;
  # ccc
  foo = {
    # ddd
    d = 3;
  };
}
",
            expect![[r#"
                {
                foo = {
                  # aaa
                  a = 1;
                  # bbb
                  b = 2;
                  # ccc

                    # ddd
                    d = 3;

                };
                }
            "#]],
        );
    }
}
