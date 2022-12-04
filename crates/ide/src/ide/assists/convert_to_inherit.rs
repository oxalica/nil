//! Convert `key = key;` into `inherit key;` in non-rec attrset.
use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use itertools::Itertools;
use syntax::ast::{self, AstNode};
use syntax::semantic::AttrKind;

pub(super) fn convert_to_inherit(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let binding = ctx.covering_node::<ast::AttrpathValue>()?;

    // Must be in non-rec attrset.
    let set = ast::AttrSet::cast(binding.syntax().parent()?)?;
    if set.rec_token().is_some() || set.let_token().is_some() {
        return None;
    }

    // RHS should be either:
    // - A single identifier.
    // - Or a select expression ending with a single (static) identifier.
    let (from_expr, rhs) = match binding.value()?.flatten_paren()? {
        ast::Expr::Ref(rhs) => ("".into(), rhs.token()?.text().into()),
        ast::Expr::Select(rhs) if rhs.or_token().is_none() => {
            let mut attrs = rhs.attrpath()?.attrs().collect::<Vec<_>>();
            let src = ctx.db.file_content(ctx.frange.file_id);
            let attr = attrs.pop()?;

            let set_range = rhs.set()?.syntax().text_range();
            let end = attrs
                .last()
                .map_or(set_range, |attr| attr.syntax().text_range())
                .end()
                .into();
            let from_expr = format!(" ({})", &src[set_range.start().into()..end]);

            let rhs = match AttrKind::of(attr) {
                AttrKind::Static(Some(rhs)) => rhs,
                _ => return None,
            };

            (from_expr, rhs)
        }
        _ => return None,
    };

    let mut attrs = binding.attrpath()?.attrs().collect::<Vec<_>>();
    let attr = attrs.pop()?;
    let key = match AttrKind::of(attr) {
        AttrKind::Static(Some(key)) => key,
        _ => return None,
    };

    // LHS should match RHS.
    if key != rhs {
        return None;
    }

    let insert = if attrs.is_empty() {
        format!("inherit{from_expr} {key};")
    } else {
        format!(
            "{} = {{ inherit{from_expr} {key}; }};",
            attrs.into_iter().map(|x| x.syntax().to_string()).join(".")
        )
    };

    // Since RHS is already a valid identifier. Not escaping is required.
    ctx.add(
        "convert_to_inherit",
        format!("Convert to `inherit{from_expr} {key}`"),
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
    fn multiple_rhs() {
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
    fn rec_attrset() {
        check_no("rec { foo $0= foo; }");
        check_no("let { foo $0= foo; }");
        check_no("let foo $0= foo; in foo");
    }
}
