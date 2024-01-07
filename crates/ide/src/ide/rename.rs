use crate::def::{AstPtr, NameId, ResolveResult};
use crate::{DefDatabase, FilePos, TextEdit, WorkspaceEdit};
use smol_str::SmolStr;
use std::borrow::Cow;
use syntax::ast::{self, AstNode};
use syntax::semantic::escape_literal_attr;
use syntax::{best_token_at_offset, match_ast, SyntaxKind, TextRange};

pub type RenameResult<T> = Result<T, String>;

pub(crate) fn prepare_rename(
    db: &dyn DefDatabase,
    fpos: FilePos,
) -> RenameResult<(TextRange, SmolStr)> {
    let (range, name) = find_name(db, fpos).ok_or_else(|| "No references found".to_owned())?;
    let module = db.module(fpos.file_id);
    let text = module[name].text.clone();
    Ok((range, text))
}

pub(crate) fn rename(
    db: &dyn DefDatabase,
    fpos: FilePos,
    new_name: &str,
) -> RenameResult<WorkspaceEdit> {
    let (_, name) = find_name(db, fpos).ok_or_else(|| "No references found".to_owned())?;

    let new_attr = escape_literal_attr(new_name);

    let file_id = fpos.file_id;
    let src = db.file_content(file_id);
    let parse = db.parse(file_id);
    let module = db.module(fpos.file_id);
    let source_map = db.source_map(file_id);

    let old_attr = escape_literal_attr(&module[name].text);

    let mut edits = Vec::new();

    // Rename definitions.
    for ptr in source_map.nodes_for_name(name) {
        let attr_node = ptr.to_node(&parse.syntax_node());

        // Simple case for non-inherited names.
        let Some(i) = attr_node.parent().and_then(ast::Inherit::cast) else {
            edits.push(TextEdit {
                delete: attr_node.text_range(),
                insert: SmolStr::new(&new_attr),
            });
            continue;
        };

        // Here we are renaming the *definition* of an inherited name.
        // `inherit old;` => `new = old;`
        //
        // Note that renaming `rec { inherit old; }` => `rec { new = old; }`
        // would never collide with another field `old`, since `inherit`ed names are unique.
        // TODO: Check if `new` collides with other fields.

        // First remove the old binding.
        edits.push(TextEdit {
            // Delete the whole Inherit if it is the only Attr.
            delete: if i.attrs().count() == 1 {
                i.syntax().text_range()
            // Otherwise, delete only the Attr itself.
            } else {
                attr_node.text_range()
            },
            insert: "".into(),
        });

        // Then construct a new binding.
        match i.from_expr() {
            None => {
                if matches!(old_attr, Cow::Owned(_)) {
                    return Err("Cannot rename from a string literal while it is inherited".into());
                }
                // `new = old;`.
                edits.push(TextEdit {
                    delete: TextRange::empty(i.syntax().text_range().end()),
                    insert: format!("{new_attr} = {old_attr};").into(),
                });
            }
            Some(from_expr) => {
                // `new = (from).old;`
                edits.push(TextEdit {
                    delete: TextRange::empty(i.syntax().text_range().end()),
                    insert: format!(
                        "{} = {}.{};",
                        new_attr,
                        // This is already parenthesized.
                        &src[from_expr.syntax().text_range()],
                        old_attr,
                    )
                    .into(),
                });
            }
        }
    }

    // Rename usages.
    let name_refs = db.name_reference(file_id);
    let refs = name_refs.name_references(name).unwrap_or_default();
    if matches!(new_attr, Cow::Owned(_)) && !refs.is_empty() {
        return Err("Cannot rename to a string literal while it is referenced".into());
    }
    for &expr in refs {
        let ptr = source_map
            .node_for_expr(expr)
            .expect("Must be a valid Expr::Reference");
        let ref_node = ptr.to_node(&parse.syntax_node());

        // Simple case for non-inherited names.
        let Some(i) = ref_node.parent().and_then(ast::Inherit::cast) else {
            edits.push(TextEdit {
                delete: ptr.text_range(),
                insert: SmolStr::new(&new_attr),
            });
            continue;
        };

        // Here we are renaming the *reference* of an inherited name.
        // `inherit old;` => `old = new;`
        // TODO: Check if `new` collides with another names.
        assert!(
            i.from_expr().is_none(),
            "Expr::Ref can only be from Inherit without from_expr"
        );

        // First remove the old binding.
        edits.push(TextEdit {
            // Delete the whole Inherit if it is the only Attr.
            delete: if i.attrs().count() == 1 {
                i.syntax().text_range()
            // Otherwise, delete only the Attr itself.
            } else {
                ref_node.text_range()
            },
            insert: "".into(),
        });

        // Then construct a new binding.
        edits.push(TextEdit {
            delete: TextRange::empty(i.syntax().text_range().end()),
            insert: format!("{old_attr} = {new_attr};").into(),
        });
    }

    edits.sort_by_key(|edit| edit.delete.start());

    // Sanity check.
    if edits
        .windows(2)
        .any(|w| w[0].delete.end() > w[1].delete.start())
    {
        return Err("Change would overlap".into());
    }

    Ok(WorkspaceEdit {
        content_edits: [(file_id, edits)].into_iter().collect(),
    })
}

fn find_name(
    db: &dyn DefDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<(TextRange, NameId)> {
    let parse = db.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;
    let mut node = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => Some(n.syntax().clone()),
                ast::Name(n) => Some(n.syntax().clone()),
                ast::String(n) => Some(n.syntax().clone()),
                ast::Dynamic(n) => Some(n.syntax().clone()),
                _ => None,
            }
        }
    })?;

    // Try to find the outermost Attr.
    // In case of `{ ${("foo")} = 1; }`
    if node.kind() == SyntaxKind::STRING
        && matches!(node.parent(), Some(p) if p.kind() == SyntaxKind::PAREN)
    {
        loop {
            node = node.parent()?;
            match node.kind() {
                SyntaxKind::DYNAMIC => break,
                SyntaxKind::PAREN => {}
                _ => return None,
            }
        }
    }
    let ptr = AstPtr::new(&node);

    let source_map = db.source_map(file_id);
    if let Some(name) = source_map.name_for_node(ptr) {
        return Some((ptr.text_range(), name));
    }

    if let Some(expr) = source_map.expr_for_node(ptr) {
        let nameres = db.name_resolution(file_id);
        if let Some(ResolveResult::Definition(name)) = nameres.get(expr) {
            return Some((ptr.text_range(), *name));
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check_prepare(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let mut src = db.file_content(f[0].file_id).to_string();
        let ret = match super::prepare_rename(&db, f[0]) {
            Ok((range, text)) => {
                let is_same = src[range] == text;
                src.insert(usize::from(range.end()), '>');
                src.insert(usize::from(range.start()), '<');
                if is_same {
                    src
                } else {
                    format!("{src}\n{text}\n")
                }
            }
            Err(err) => err,
        };
        expect.assert_eq(&ret);
    }

    fn check(fixture: &str, new_name: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let mut src = db.file_content(f[0].file_id).to_string();
        let ret = match super::rename(&db, f[0], new_name) {
            Ok(ws_edit) => {
                let edits = ws_edit.content_edits.into_iter().collect::<Vec<_>>();
                assert_eq!(edits[0].0, f[0].file_id);
                for edit in edits[0].1.iter().rev() {
                    edit.apply(&mut src);
                }
                src
            }
            Err(err) => err,
        };
        expect.assert_eq(&ret);
    }

    #[test]
    fn prepare_ident() {
        check_prepare("let $0a = a; in a", expect!["let <a> = a; in a"]);
        check_prepare("let a = $0a; in a", expect!["let a = <a>; in a"]);
        check_prepare("{ $0a = 1; }", expect!["{ <a> = 1; }"]);
        check_prepare("rec { $0a = 1; }", expect!["rec { <a> = 1; }"]);
        check_prepare("{ a.$0b.c = 1; }", expect!["{ a.<b>.c = 1; }"]);
    }

    #[test]
    fn prepare_string() {
        check_prepare(
            r#"let $0"a" = a; in a"#,
            expect![[r#"
                let <"a"> = a; in a
                a
            "#]],
        );
        check_prepare(
            r#"let a = a; in { inherit $0"a"; }"#,
            expect![[r#"
                let a = a; in { inherit <"a">; }
                a
            "#]],
        );
    }

    #[test]
    fn prepare_dynamic() {
        check_prepare(
            r#"let ${(($0"a"))} = a; in a"#,
            expect![[r#"
                let <${(("a"))}> = a; in a
                a
            "#]],
        );
        check_prepare(
            r#"let ${($0("a"))} = a; in a"#,
            expect![[r#"
                let <${(("a"))}> = a; in a
                a
            "#]],
        );
        check_prepare(
            r#"let $0${(("a"))} = a; in a"#,
            expect![[r#"
                let <${(("a"))}> = a; in a
                a
            "#]],
        );
        check_prepare(
            r#"let a = a; in { inherit ${(($0"a"))}; }"#,
            expect![[r#"
                let a = a; in { inherit <${(("a"))}>; }
                a
            "#]],
        );
    }

    #[test]
    fn rename_let_ident() {
        check(
            "let $0a = a; in { a = a; }",
            "b",
            expect!["let b = b; in { a = b; }"],
        );
        check(
            "let a = $0a; in { a = a; }",
            "b",
            expect!["let b = b; in { a = b; }"],
        );
    }

    #[test]
    fn rename_let_string() {
        check(
            r#"let $0"a" = a; in { a = a; }"#,
            "b",
            expect!["let b = b; in { a = b; }"],
        );
        check(
            r#"let "a" = $0a; in { a = a; }"#,
            "b",
            expect!["let b = b; in { a = b; }"],
        );
    }

    #[test]
    fn rename_let_dynamic() {
        check(
            r#"let $0${"a"} = a; in { a = a; }"#,
            "b",
            expect!["let b = b; in { a = b; }"],
        );
        check(
            r#"let ${"a"} = $0a; in { a = a; }"#,
            "b",
            expect!["let b = b; in { a = b; }"],
        );
    }

    #[test]
    fn rename_plain_attrset() {
        check(
            "let a = 1; in { $0a = a; }",
            "b",
            expect!["let a = 1; in { b = a; }"],
        );
    }

    #[test]
    fn rename_rec_attrset() {
        check(
            "let a = 1; in rec { $0a = a; }",
            "b",
            expect!["let a = 1; in rec { b = b; }"],
        );
    }

    #[test]
    fn rename_lambda_param() {
        check(
            "{ $0a ? b, b ? a }@c: c",
            "x",
            expect!["{ x ? b, b ? x }@c: c"],
        );
        check(
            "{ a ? $0b, b ? a }@c: c",
            "x",
            expect!["{ a ? x, x ? a }@c: c"],
        );
        check(
            "{ a ? b, b ? a }@$0c: c",
            "x",
            expect!["{ a ? b, b ? a }@x: x"],
        );
        check(
            "{ a ? b, b ? a }@c: $0c",
            "x",
            expect!["{ a ? b, b ? a }@x: x"],
        );
    }

    #[test]
    fn rename_merged() {
        check(
            "{ a.b.c = 1; a.$0b.d = 2; a.b = { c = 3; }; }",
            "x",
            expect!["{ a.x.c = 1; a.x.d = 2; a.x = { c = 3; }; }"],
        );

        check(
            "{ a = rec { $0b = c; }; a.c = b; }",
            "x",
            expect!["{ a = rec { x = c; }; a.c = x; }"],
        );
        check(
            "{ a = rec { b = $0c; }; a.c = b; }",
            "x",
            expect!["{ a = rec { b = x; }; a.x = b; }"],
        );
    }

    #[test]
    fn rename_to_string() {
        check("{ $0a = 1; }", "1", expect![[r#"{ "1" = 1; }"#]]);
        check(
            "{ $0a.a = 1; a.b = 1; }",
            "1",
            expect![[r#"{ "1".a = 1; "1".b = 1; }"#]],
        );
        check("let $0a = 1; in 1", "1", expect![[r#"let "1" = 1; in 1"#]]);
        check(
            "let $0a = 1; in a",
            "1",
            expect!["Cannot rename to a string literal while it is referenced"],
        );
        check(
            "rec { $0a = a; }",
            "1",
            expect!["Cannot rename to a string literal while it is referenced"],
        );
    }

    #[test]
    fn rename_inherit_simple_definition() {
        check(
            r#"let a = 1; in { inherit $0a; }"#,
            "b",
            expect!["let a = 1; in { b = a; }"],
        );
        check(
            r#"let a = 1; in { inherit $0a x; }"#,
            "b",
            expect!["let a = 1; in { inherit  x;b = a; }"],
        );
        check(
            r#"let a = 1; in { inherit $0a; }"#,
            "1",
            expect![[r#"let a = 1; in { "1" = a; }"#]],
        );
        check(
            r#"let "1" = 1; in { inherit $0"1"; }"#,
            "b",
            expect!["Cannot rename from a string literal while it is inherited"],
        );
    }

    #[test]
    fn rename_inherit_from_definition() {
        check(r#"{ inherit (1) $0a; }"#, "b", expect!["{ b = (1).a; }"]);
        check(
            r#"{ inherit (1) $0a x; }"#,
            "b",
            expect!["{ inherit (1)  x;b = (1).a; }"],
        );
        check(
            r#"{ inherit (1) $0a; }"#,
            "1",
            expect![[r#"{ "1" = (1).a; }"#]],
        );
        check(
            r#"{ inherit (1) $0"1"; }"#,
            "b",
            expect![[r#"{ b = (1)."1"; }"#]],
        );
    }

    #[test]
    fn rename_inherit_simple_reference() {
        check(
            r#"let $0a = 1; in { inherit a; }"#,
            "b",
            expect!["let b = 1; in { a = b; }"],
        );
        check(
            r#"let $0a = 1; in { inherit a x; }"#,
            "b",
            expect!["let b = 1; in { inherit  x;a = b; }"],
        );
        check(
            r#"let $0a = 1; in { inherit a; }"#,
            "1",
            expect!["Cannot rename to a string literal while it is referenced"],
        );
        check(
            r#"let $0"1" = 1; in { inherit "1"; }"#,
            "b",
            expect![[r#"let b = 1; in { "1" = b; }"#]],
        );
    }
}
