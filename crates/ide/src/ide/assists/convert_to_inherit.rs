//! Convert `key = key;` into `inherit key;` in non-rec attrset.
use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use rowan::ast::AstNode;
use syntax::ast;
use syntax::semantic::AttrKind;

pub(super) fn convert_to_inherit(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let binding = ctx.covering_node::<ast::AttrpathValue>()?;

    // Must be in non-rec attrset.
    let set = ast::AttrSet::cast(binding.syntax().parent()?)?;
    if set.rec_token().is_some() || set.let_token().is_some() {
        return None;
    }

    // RHS should be a single identifier.
    let rhs = match binding.value()?.flatten_paren()? {
        ast::Expr::Ref(rhs) => rhs,
        _ => return None,
    };

    // LHS should be a single static name.
    let mut attrs = binding.attrpath()?.attrs();
    let attr = attrs.next()?;
    if attrs.next().is_some() {
        return None;
    }
    let key = match AttrKind::of(attr) {
        AttrKind::Static(Some(key)) => key,
        _ => return None,
    };

    // LHS should match RHS.
    if key != rhs.token()?.text() {
        return None;
    }

    // Since RHS is already a valid identifier. Not escaping is required.
    ctx.add(
        "convert_to_inherit",
        format!("Convert to `inherit {key}`"),
        AssistKind::RefactorRewrite,
        vec![TextEdit {
            delete: binding.syntax().text_range(),
            insert: format!("inherit {key};").into(),
        }],
    );

    Some(())
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    #[test]
    fn simple() {
        check_assist(convert_to_inherit, "{ $0foo = foo; }", "{ inherit foo; }");
        check_assist(convert_to_inherit, "{ f$0oo = foo; }", "{ inherit foo; }");
        check_assist(convert_to_inherit, "{ foo $0= foo; }", "{ inherit foo; }");
        check_assist(convert_to_inherit, "{ foo = f$0oo; }", "{ inherit foo; }");
        check_assist(convert_to_inherit, "{ fo$0o = fo$1o; }", "{ inherit foo; }");

        check_assist_no(convert_to_inherit, "$0{ foo = foo; }");
    }

    #[test]
    fn nested() {
        check_assist(
            convert_to_inherit,
            r#"{ ${("foo")} = (($0foo)); }"#,
            "{ inherit foo; }",
        );
    }

    #[test]
    fn simple_no() {
        check_assist_no(convert_to_inherit, "{ foo $0= bar; }");
        check_assist_no(convert_to_inherit, "{ foo.foo $0= foo; }");
    }

    #[test]
    fn rec_attrset() {
        check_assist_no(convert_to_inherit, "rec { foo $0= foo; }");
        check_assist_no(convert_to_inherit, "let { foo $0= foo; }");
        check_assist_no(convert_to_inherit, "let foo $0= foo; in foo");
    }
}
