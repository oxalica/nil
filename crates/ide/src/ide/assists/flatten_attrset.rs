//! Flatten binding with Attrset RHS into multiple bindings of outer level.
//! FIXME: Indentations are not reformated well.
//!
//! ```nix
//! {
//!   foo = {
//!     bar = 1;
//!     baz = 2;
//!   };
//! }
//! ```
//! =>
//! ```nix
//! {
//!   foo.bar = 1;
//!   foo.baz = 2;
//! }
//! ```
use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use itertools::Itertools;
use rowan::ast::AstNode;
use rowan::TextRange;
use syntax::ast::{self, HasBindings};

pub(super) fn flatten_attrset(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    // Matches `attr.path = { ... };`.
    let path_value = ctx.covering_node::<ast::AttrpathValue>()?;
    let value = path_value.value()?.flatten_paren()?;
    let set = match value {
        ast::Expr::AttrSet(set) if set.let_token().is_none() && set.rec_token().is_none() => set,
        _ => return None,
    };

    // Cannot flatten empty Attrset.
    set.bindings().next()?;

    let src = ctx.db.file_content(ctx.frange.file_id);
    let prefix_path = &src[path_value.attrpath()?.syntax().text_range()].trim();

    let open_range = TextRange::new(
        path_value.syntax().text_range().start(),
        set.l_curly_token()?.text_range().end(),
    );
    let close_range = TextRange::new(
        set.r_curly_token()?.text_range().start(),
        path_value.syntax().text_range().end(),
    );
    let mut edits = vec![
        TextEdit {
            delete: open_range,
            insert: "".into(),
        },
        TextEdit {
            delete: close_range,
            insert: "".into(),
        },
    ];

    for b in set.bindings() {
        match b {
            ast::Binding::AttrpathValue(path_value) => {
                let start_pos = path_value.syntax().text_range().start();
                edits.push(TextEdit {
                    delete: TextRange::empty(start_pos),
                    insert: format!("{prefix_path}.").into(),
                });
            }
            ast::Binding::Inherit(i) => {
                let from_expr = i.from_expr().map(|e| &src[e.syntax().text_range()]);
                let replace = i
                    .attrs()
                    .map(|attr| {
                        let attr = &src[attr.syntax().text_range()];
                        match from_expr {
                            Some(e) => format!("{prefix_path}.{attr} = {e}.{attr};"),
                            None => format!("{prefix_path}.{attr} = {attr};"),
                        }
                    })
                    .join("\n");
                edits.push(TextEdit {
                    delete: i.syntax().text_range(),
                    insert: replace.into(),
                });
            }
        }
    }

    ctx.add(
        "flatten_attrset",
        "Flatten Attrset RHS into outer level bindings",
        AssistKind::RefactorRewrite,
        edits,
    );
    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    define_check_assist!(super::flatten_attrset);

    #[test]
    fn no_empty() {
        check_no("{ $0foo = { }; }");
    }

    #[test]
    fn no_rec() {
        check_no("{ $0foo = rec { bar = 1; }; }");
    }

    #[test]
    fn single() {
        check("{ $0foo = { a = 1; }; }", expect!["{  foo.a = 1;  }"]);
        check(
            "
{
  foo.${bar$0}.baz = {
    a = 1;
  };
}",
            expect![[r#"
                {

                    foo.${bar}.baz.a = 1;

                }
            "#]],
        );
    }

    #[test]
    fn mixed() {
        check(
            "
{
  $0foo = {
    a = 1;
    inherit b c;
    d = 2;
    inherit;
    inherit (d) e f;
    ${g} = 3;
  };
}
            ",
            expect![[r#"
                {

                    foo.a = 1;
                    foo.b = b;
                foo.c = c;
                    foo.d = 2;

                    foo.e = (d).e;
                foo.f = (d).f;
                    foo.${g} = 3;

                }
            "#]],
        );
    }
}
