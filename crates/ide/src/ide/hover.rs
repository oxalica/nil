use crate::def::{AstPtr, Expr, ResolveResult};
use crate::ty::{DisplayConfig, Ty};
use crate::{FilePos, NameKind, TyDatabase};
use builtin::ALL_BUILTINS;
use if_chain::if_chain;
use std::fmt::Write;
use syntax::ast::{self, AstNode};
use syntax::semantic::AttrKind;
use syntax::{best_token_at_offset, match_ast, TextRange};

// Kinda detailed, but don't flood users with thousands of fields for `pkgs`.
pub const TY_DETAILED_DISPLAY: DisplayConfig = DisplayConfig {
    max_lambda_lhs_depth: 4,
    max_list_depth: 4,
    max_attrset_depth: 2,
    max_attrset_fields: 4,
    lambda_need_parentheses: false,
};

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

    if let Some(expr) = source_map.expr_for_node(ptr) {
        if let Some(builtin) = nameres.check_builtin(expr, &module) {
            return hover_builtin(builtin, range);
        }

        match nameres.get(expr) {
            None => {}
            // Covered by `check_builtin`.
            Some(ResolveResult::Builtin(_)) => unreachable!(),
            Some(ResolveResult::WithExprs(withs)) => {
                let Expr::Reference(text) = &module[expr] else {
                    return None;
                };
                let ty = infer
                    .ty_for_expr(expr)
                    .display_with(TY_DETAILED_DISPLAY)
                    .to_string();
                let mut markup = format!("`with` attribute `{text}`\n`{ty}`\nEnvironments:");
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

    if let Some(name) = name.or_else(|| source_map.name_for_node(ptr)) {
        let ty = infer
            .ty_for_name(name)
            .display_with(TY_DETAILED_DISPLAY)
            .to_string();
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
            markup: format!("{kind} `{text}`\n`{ty}`"),
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

        // Special case for `builtins.xxx`
        if_chain! {
            if let Some(ResolveResult::Builtin("builtins")) = nameres.get(expr);
            if let Some(attr) = path_node.attrs().next();
            if let AttrKind::Static(Some(field)) = AttrKind::of(attr.clone());
            if ALL_BUILTINS.contains_key(&field);
            then {
                // `builtins.xxx.other`
                //  ^^^^^^^^^^^^
                let range = set_node
                    .syntax()
                    .text_range()
                    .cover(attr.syntax().text_range());
                // NB. Returns None when the field is invalid,
                // since it is known to be incorrect.
                return hover_builtin(&field, range);
            }
        }

        let mut ty = infer.ty_for_expr(expr);
        for attr in path_node.attrs() {
            let AttrKind::Static(Some(field)) = AttrKind::of(attr.clone()) else {
                return None;
            };
            ty = ty.as_attrset()?.get(&field)?.clone();
            if attr.syntax() == name_node.syntax() {
                break;
            }
        }
        let range = name_node.syntax().text_range();
        let markup = format!(
            "Field `{}`\n`{}`",
            name_node
                .token()
                .map_or_else(String::new, |t| t.text().into()),
            ty.display_with(TY_DETAILED_DISPLAY),
        );
        Some(HoverResult { range, markup })
    }) {
        return Some(ret);
    }

    None
}

fn hover_builtin(name: &str, range: TextRange) -> Option<HoverResult> {
    let b = ALL_BUILTINS.get(name)?;
    let ty = crate::ty::known::BUILTINS
        .as_attrset()
        .unwrap()
        .get(name)
        .cloned()
        .unwrap_or(Ty::Unknown);
    let markup = format!(
        "`builtins.{name}`\n`{}`\n\n{}\n{}",
        ty.display_with(TY_DETAILED_DISPLAY),
        b.summary,
        b.doc.unwrap_or("(No documentation from Nix)"),
    );
    Some(HoverResult { range, markup })
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

    #[track_caller]
    fn check_no(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        assert_eq!(super::hover(&db, f[0]), None);
    }

    #[test]
    fn definition() {
        check(
            "let $0a = 1; in a",
            "a",
            expect![[r#"
                Let binding `a`
                `int`
            "#]],
        );
        check(
            "let a.$0a = 1; in a",
            "a",
            expect![[r#"
                Attrset attribute `a`
                `int`
            "#]],
        );
        check(
            "{ $0a = 1; }",
            "a",
            expect![[r#"
                Attrset attribute `a`
                `int`
            "#]],
        );
        check(
            "rec { $0a = 1; }",
            "a",
            expect![[r#"
                Rec-attrset attribute `a`
                `int`
            "#]],
        );
        check(
            "$0a: a",
            "a",
            expect![[r#"
                Parameter `a`
                `?`
            "#]],
        );
        check(
            "{$0a}: a",
            "a",
            expect![[r#"
                Field parameter `a`
                `?`
            "#]],
        );
    }

    #[test]
    fn reference() {
        check(
            "let a = 1; in $0a",
            "a",
            expect![[r#"
                Let binding `a`
                `int`
            "#]],
        );
        check(
            "let a = 1; in { inherit $0a; }",
            "a",
            expect![[r#"
                Let binding `a`
                `int`
            "#]],
        );
        check(
            "let a = 1; in rec { inherit $0a; }",
            "a",
            expect![[r#"
                Let binding `a`
                `int`
            "#]],
        );
        check(
            "a: $0a",
            "a",
            expect![[r#"
                Parameter `a`
                `?`
            "#]],
        );
        check(
            "{a}: $0a",
            "a",
            expect![[r#"
                Field parameter `a`
                `?`
            "#]],
        );
    }

    #[test]
    fn with() {
        check(
            "with 1; $0a",
            "a",
            expect![[r#"
                `with` attribute `a`
                `?`
                Environments:
                1. `with 1;`
            "#]],
        );
        check(
            "with 1; with 2; $0a",
            "a",
            expect![[r#"
                `with` attribute `a`
                `?`
                Environments:
                1. `with 2;`
                2. `with 1;`
            "#]],
        );
    }

    #[test]
    #[ignore = "asserts on nix docs"]
    fn builtin_global() {
        check(
            "$0true",
            "true",
            expect![[r#"
                `builtins.true`
                `bool`

                `builtins.true`
                Primitive value.

                It can be returned by
                [comparison operators](@docroot@/language/operators.md#Comparison)
                and used in
                [conditional expressions](@docroot@/language/constructs.md#Conditionals).

                The name `true` is not special, and can be shadowed:

                ```nix-repl
                nix-repl> let true = 1; in true
                1
                ```
            "#]],
        );
        check(
            "$0map",
            "map",
            expect![[r#"
                `builtins.map`
                `(? → ?) → [?] → [?]`

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
    fn builtin_with() {
        check(
            "with { }; with builtins; head$0",
            "head",
            expect![[r#"
                `builtins.head`
                `[?] → ?`

                `builtins.head list`
                Return the first element of a list; abort evaluation if the argument
                isn’t a list or is an empty list. You can test whether a list is
                empty by comparing it with `[]`.
            "#]],
        );
    }

    #[test]
    fn builtin_alias() {
        check(
            "let inherit (builtins) head; in head$0",
            "head",
            expect![[r#"
                `builtins.head`
                `[?] → ?`

                `builtins.head list`
                Return the first element of a list; abort evaluation if the argument
                isn’t a list or is an empty list. You can test whether a list is
                empty by comparing it with `[]`.
            "#]],
        );
    }

    #[test]
    #[ignore = "asserts on nix docs"]
    fn builtin_attrpath() {
        check(
            "builtins.head$0",
            "builtins.head",
            expect![[r#"
                `builtins.head`
                `[?] → ?`

                `builtins.head list`
                Return the first element of a list; abort evaluation if the argument
                isn’t a list or is an empty list. You can test whether a list is
                empty by comparing it with `[]`.
            "#]],
        );

        check(
            "builtins.true$0.trailing",
            "builtins.true",
            expect![[r#"
                `builtins.true`
                `bool`

                `builtins.true`
                Primitive value.

                It can be returned by
                [comparison operators](@docroot@/language/operators.md#Comparison)
                and used in
                [conditional expressions](@docroot@/language/constructs.md#Conditionals).

                The name `true` is not special, and can be shadowed:

                ```nix-repl
                nix-repl> let true = 1; in true
                1
                ```
            "#]],
        );

        // Invalid builtins.
        check_no("builtins.not_exist$0");
        // But the first part still works.
        check(
            "builtins$0.not_exist",
            "builtins",
            expect![[r#"
                `builtins.builtins`
                `{ abort: string → ?, add: float → float → float, addErrorContext: string → ? → ?, all: (? → bool) → [?] → bool, … }`

                `builtins.builtins`
                Contains all the [built-in functions](@docroot@/language/builtins.md) and values.

                Since built-in functions were added over time, [testing for attributes](./operators.md#has-attribute) in `builtins` can be used for graceful fallback on older Nix installations:

                ```nix
                # if hasContext is not available, we assume `s` has a context
                if builtins ? hasContext then builtins.hasContext s else true
                ```
            "#]],
        );
    }

    #[test]
    fn attrpath() {
        check(
            "let foo.$0bar = 1; in foo.bar",
            "bar",
            expect![[r#"
                Attrset attribute `bar`
                `int`
            "#]],
        );
        check(
            "let foo.bar = 1; in foo.$0bar",
            "bar",
            expect![[r#"
                Field `bar`
                `int`
            "#]],
        );
        check(
            "let foo.bar = 1; in foo?$0bar",
            "bar",
            expect![[r#"
                Field `bar`
                `int`
            "#]],
        );
    }
}
