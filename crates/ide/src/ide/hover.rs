use crate::def::{AstPtr, Expr, ResolveResult};
use crate::{FilePos, NameKind, TyDatabase};
use builtin::ALL_BUILTINS;
use std::fmt::Write;
use syntax::ast::{self, AstNode};
use syntax::semantic::AttrKind;
use syntax::{best_token_at_offset, match_ast, TextRange};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoverResult {
    pub range: TextRange,
    pub markup: String,
}

pub(crate) fn hover(db: &dyn TyDatabase, FilePos { file_id, pos }: FilePos) -> Option<HoverResult> {
    let parse = db.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;
    let mut name_node = None;
    let ptr = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => Some(AstPtr::new(n.syntax())),
                ast::Name(n) => {
                    let ptr = AstPtr::new(n.syntax());
                    name_node = Some(n);
                    Some(ptr)
                },
                ast::Literal(n) => Some(AstPtr::new(n.syntax())),
                _ => None,
            }
        }
    })?;
    let range = ptr.text_range();

    let src = db.file_content(file_id);
    let module = db.module(file_id);
    let source_map = db.source_map(file_id);
    let nameres = db.name_resolution(file_id);
    let infer = db.infer(file_id);

    let mut name = None;

    if let Some(expr) = source_map.expr_for_node(ptr.clone()) {
        match nameres.get(expr) {
            None => {}
            Some(ResolveResult::Builtin(name)) => {
                let b = &ALL_BUILTINS[*name];
                // TODO: Types of builtins.
                let markup = format!(
                    "{}\n\n{}",
                    b.summary,
                    b.doc.unwrap_or("(No documentation from Nix)"),
                );
                return Some(HoverResult { range, markup });
            }
            Some(ResolveResult::WithExprs(withs)) => {
                let text = match &module[expr] {
                    Expr::Reference(text) => text,
                    _ => return None,
                };
                let ty = infer.ty_for_expr(expr).display().to_string();
                let mut markup = format!("`with` attribute `{text}`: `{ty}`");
                for (&expr, i) in withs.iter().zip(1..) {
                    let ptr = source_map.node_for_expr(expr)?;
                    let with_node = ast::With::cast(ptr.to_node(&parse.syntax_node()))?;
                    let env_text = with_node
                        .environment()
                        .map_or("?", |env_node| &src[env_node.syntax().text_range()]);
                    write!(markup, "\n{i}. `with {env_text};`").unwrap();
                }
                return Some(HoverResult { range, markup });
            }
            Some(ResolveResult::Definition(def)) => {
                name = Some(*def);
            }
        }
    }

    if let Some(name) = name.or_else(|| source_map.name_for_node(ptr.clone())) {
        let ty = infer.ty_for_name(name).display().to_string();
        let text = &module[name].text;
        let kind = match module[name].kind {
            NameKind::LetIn => "Let binding",
            NameKind::PlainAttrset => "Attrset attribute",
            NameKind::RecAttrset => "Rec-attrset attribute",
            NameKind::Param => "Parameter",
            NameKind::PatField => "Field parameter",
        };
        return Some(HoverResult {
            range,
            markup: format!("{kind} `{text}`: `{ty}`"),
        });
    }

    // Selected attr type.
    // `let a.b.c = 1; in a.b.c`
    //                      ^ { c: int }
    if let Some(ret) = name_node.and_then(|name_node| {
        let path_node = ast::Attrpath::cast(name_node.syntax().parent()?)?;
        let set_node = match_ast! {
            match (path_node.syntax().parent()?) {
                ast::HasAttr(n) => n.set(),
                ast::Select(n) => n.set(),
                _ => None,
            }
        }?;
        let expr = source_map.expr_for_node(AstPtr::new(set_node.syntax()))?;
        let mut ty = infer.ty_for_expr(expr);
        for attr in path_node.attrs() {
            let AttrKind::Static(Some(field)) = AttrKind::of(attr.clone()) else { return None };
            ty = ty.as_attrset()?.get(&field)?.clone();
            if attr.syntax() == name_node.syntax() {
                break;
            }
        }
        let range = name_node.syntax().text_range();
        let markup = format!(
            "Field `{}`: `{}`",
            name_node
                .token()
                .map_or_else(String::new, |t| t.text().into()),
            ty.display(),
        );
        Some(HoverResult { range, markup })
    }) {
        return Some(ret);
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, full: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        let ret = super::hover(&db, f[0]).expect("No hover");
        let src = db.file_content(f[0].file_id);
        assert_eq!(full, &src[ret.range]);
        let mut got = ret.markup.trim().to_string();
        if got.contains('\n') {
            got += "\n";
        }
        expect.assert_eq(&got);
    }

    #[test]
    fn definition() {
        check("let $0a = 1; in a", "a", expect!["Let binding `a`: `int`"]);
        check(
            "let a.$0a = 1; in a",
            "a",
            expect!["Attrset attribute `a`: `int`"],
        );
        check("{ $0a = 1; }", "a", expect!["Attrset attribute `a`: `int`"]);
        check(
            "rec { $0a = 1; }",
            "a",
            expect!["Rec-attrset attribute `a`: `int`"],
        );
        check("$0a: a", "a", expect!["Parameter `a`: `?`"]);
        check("{$0a}: a", "a", expect!["Field parameter `a`: `?`"]);
    }

    #[test]
    fn reference() {
        check("let a = 1; in $0a", "a", expect!["Let binding `a`: `int`"]);
        check(
            "let a = 1; in { inherit $0a; }",
            "a",
            expect!["Let binding `a`: `int`"],
        );
        check(
            "let a = 1; in rec { inherit $0a; }",
            "a",
            expect!["Let binding `a`: `int`"],
        );
        check("a: $0a", "a", expect!["Parameter `a`: `?`"]);
        check("{a}: $0a", "a", expect!["Field parameter `a`: `?`"]);
    }

    #[test]
    fn with() {
        check(
            "with 1; $0a",
            "a",
            expect![[r#"
                `with` attribute `a`: `?`
                1. `with 1;`
            "#]],
        );
        check(
            "with 1; with 2; $0a",
            "a",
            expect![[r#"
                `with` attribute `a`: `?`
                1. `with 2;`
                2. `with 1;`
            "#]],
        );
    }

    #[test]
    fn builtin() {
        check(
            "$0true",
            "true",
            expect![[r#"
                `builtins.true`

                (No documentation from Nix)
            "#]],
        );
        check(
            "$0map",
            "map",
            expect![[r#"
                `builtins.map f list`

                Apply the function *f* to each element in the list *list*. For
                example,

                ```nix
                map (x: "foo" + x) [ "bar" "bla" "abc" ]
                ```

                evaluates to `[ "foobar" "foobla" "fooabc" ]`.
            "#]],
        );
    }

    #[test]
    fn attrpath() {
        check(
            "let foo.$0bar = 1; in foo.bar",
            "bar",
            expect!["Attrset attribute `bar`: `int`"],
        );
        check(
            "let foo.bar = 1; in foo.$0bar",
            "bar",
            expect!["Field `bar`: `int`"],
        );
        check(
            "let foo.bar = 1; in foo?$0bar",
            "bar",
            expect!["Field `bar`: `int`"],
        );
    }
}
