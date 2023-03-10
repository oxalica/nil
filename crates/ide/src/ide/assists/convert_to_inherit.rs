//! Convert `path = value;` into `inherit key;`.
//! This covers,
//! - `prefix.key = key;` => `prefix = { inherit key; };`
//!   Here the `prefix` set mut not be `rec`. The code before is actually
//!   an infinite recursion while the code after is not.
//! - `prefix.key = from.key;` => `prefix = [rec] { inherit (from) key; };`
//!   Since the `from` is resolved in the `prefix` scope thus
//!   it is allowed to have recursive references (but may not be infinite recursion).
use super::{AssistKind, AssistsCtx};
use crate::def::AstPtr;
use crate::{NameKind, TextEdit};
use itertools::Itertools;
use syntax::ast::{self, AstNode};
use syntax::semantic::AttrKind;

pub(super) fn convert_to_inherit(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let binding = ctx.covering_node::<ast::AttrpathValue>()?;

    let src = ctx.db.file_content(ctx.frange.file_id);

    // RHS should be either:
    // - A single identifier.
    // - Or a select expression ending with a single (static) identifier.
    let (from_frag, rhs_name) = match binding.value()?.flatten_paren()? {
        ast::Expr::Ref(rhs) => (String::new(), rhs.token()?.text().to_owned()),
        ast::Expr::Select(rhs) if rhs.or_token().is_none() => {
            let mut attrs = rhs.attrpath()?.attrs().collect::<Vec<_>>();
            let last_attr = attrs.pop()?;

            let set_range = rhs.set()?.syntax().text_range();
            let from_expr_range = attrs.last().map_or(set_range, |attr| {
                set_range.cover(attr.syntax().text_range())
            });
            let from_expr = format!(" ({})", &src[from_expr_range]);

            let AttrKind::Static(Some(ident)) = AttrKind::of(last_attr) else { return None };

            (from_expr, ident)
        }
        _ => return None,
    };

    let module = ctx.db.module(ctx.frange.file_id);
    let source_map = ctx.db.source_map(ctx.frange.file_id);

    let mut attrs = binding.attrpath()?.attrs().collect::<Vec<_>>();
    let last_attr = attrs.pop()?;
    let lhs_name = source_map.name_for_node(AstPtr::new(last_attr.syntax()))?;
    let is_rec = match module[lhs_name].kind {
        NameKind::LetIn | NameKind::RecAttrset => true,
        NameKind::PlainAttrset => false,
        _ => return None,
    };

    // LHS should match RHS.
    if module[lhs_name].text != rhs_name {
        return None;
    }

    // Ignore direct recursion `rec { foo = foo; }`.
    if is_rec && from_frag.is_empty() {
        return None;
    }

    let insert = if attrs.is_empty() {
        format!("inherit{from_frag} {rhs_name};")
    } else {
        format!(
            "{} = {{ inherit{from_frag} {rhs_name}; }};",
            attrs.into_iter().map(|x| x.syntax().to_string()).join(".")
        )
    };

    // Since RHS is already a valid identifier. Not escaping is required.
    ctx.add(
        "convert_to_inherit",
        format!("Convert to `inherit{from_frag} {rhs_name}`"),
        AssistKind::RefactorRewrite,
        vec![TextEdit {
            delete: binding.syntax().text_range(),
            insert: insert.into(),
        }],
    );

    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    define_check_assist!(super::convert_to_inherit);

    #[test]
    fn simple() {
        check("{ $0foo = foo; }", expect!["{ inherit foo; }"]);
        check("{ f$0oo = foo; }", expect!["{ inherit foo; }"]);
        check("{ foo $0= foo; }", expect!["{ inherit foo; }"]);
        check("{ foo = f$0oo; }", expect!["{ inherit foo; }"]);
        check("{ fo$0o = fo$1o; }", expect!["{ inherit foo; }"]);

        check_no("$0{ foo = foo; }");
    }

    #[test]
    fn multiple_lhs() {
        check(
            "{ foo.bar$0 = bar; }",
            expect!["{ foo = { inherit bar; }; }"],
        );
        check(
            r#"{ foo.${"bar"}.baz = baz$0; }"#,
            expect![r#"{ foo.${"bar"} = { inherit baz; }; }"#],
        );
    }

    #[test]
    fn multiple_rhs_plain() {
        check("{ foo = bar.foo$0; }", expect!["{ inherit (bar) foo; }"]);
        check(
            "{ foo.bar = ba$0z.bar; }",
            expect!["{ foo = { inherit (baz) bar; }; }"],
        );
        check(
            "{ foo.bar.baz $0= qux.foo.baz; }",
            expect!["{ foo.bar = { inherit (qux.foo) baz; }; }"],
        );
        check(
            r#"{ $0foo = bar.${let baz = "qux"; in baz}.foo; }"#,
            expect![r#"{ inherit (bar.${let baz = "qux"; in baz}) foo; }"#],
        );

        // Actually not rec.
        check(
            "rec { bar = { }; foo.bar $0= bar; }",
            expect!["rec { bar = { }; foo = { inherit bar; }; }"],
        );
        check(
            "let bar = { }; foo.bar $0= bar; in foo",
            expect!["let bar = { }; foo = { inherit bar; }; in foo"],
        );
    }

    #[test]
    fn nested() {
        check(
            r#"{ ${("foo")} = (($0foo)); }"#,
            expect!["{ inherit foo; }"],
        );
    }

    #[test]
    fn simple_no() {
        check_no("{ foo $0= bar; }");
        check_no("{ foo.bar $0= foo; }");
    }

    #[test]
    fn no_direct_recursion() {
        check_no("rec { foo $0= foo; }");
        check_no("let { foo $0= foo; }");
        check_no("let foo $0= foo; in foo");
        check_no("{ foo = rec { }; foo.bar $0= bar; }");
    }

    #[test]
    fn multiple_rhs_rec() {
        check(
            "let bar = { }; foo $0= bar.foo; in foo",
            expect!["let bar = { }; inherit (bar) foo; in foo"],
        );
        check(
            "rec { bar = { }; foo $0= bar.foo; }",
            expect!["rec { bar = { }; inherit (bar) foo; }"],
        );

        check(
            "let foo $0= foo.foo; in foo",
            expect!["let inherit (foo) foo; in foo"],
        );
    }
}
