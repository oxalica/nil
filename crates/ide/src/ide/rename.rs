use crate::def::{AstPtr, NameId, ResolveResult};
use crate::{DefDatabase, FilePos, TextEdit, WorkspaceEdit};
use rowan::ast::AstNode;
use smol_str::SmolStr;
use syntax::{ast, best_token_at_offset, match_ast, SyntaxKind, TextRange};

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
    if !is_valid_ident(new_name) {
        return Err("Invalid new identifier".into());
    }

    let file_id = fpos.file_id;
    let parse = db.parse(file_id);
    let source_map = db.source_map(file_id);

    let mut edits = Vec::new();
    let new_name = SmolStr::from(new_name);

    // Rename definitions.
    for ptr in source_map.name_nodes(name) {
        let node = ptr.to_node(&parse.syntax_node());
        if matches!(node.parent(), Some(p) if p.kind() == SyntaxKind::INHERIT) {
            return Err("Renaming `inherit`ed variables is not supported yet".into());
        }
        edits.push(TextEdit {
            delete: node.text_range(),
            insert: new_name.clone(),
        });
    }

    // Rename usages.
    let name_refs = db.name_reference(file_id);
    let refs = name_refs.name_references(name).unwrap_or_default();
    for &expr in refs {
        let ptr = source_map
            .expr_node(expr)
            .expect("Must be a valid Expr::Reference");
        let node = ptr.to_node(&parse.syntax_node());
        if matches!(node.parent(), Some(p) if p.kind() == SyntaxKind::INHERIT) {
            return Err("Renaming variables being `inherit`ed is not supported yet".into());
        }
        edits.push(TextEdit {
            delete: ptr.text_range(),
            insert: new_name.clone(),
        });
    }

    edits.sort_by_key(|edit| edit.delete.start());
    assert!(
        edits
            .windows(2)
            .all(|w| w[0].delete.end() <= w[1].delete.start()),
        "Should not overlap"
    );

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
    if let Some(name) = source_map.node_name(ptr.clone()) {
        return Some((ptr.text_range(), name));
    }

    if let Some(expr) = source_map.node_expr(ptr.clone()) {
        let nameres = db.name_resolution(file_id);
        if let Some(ResolveResult::Definition(name)) = nameres.get(expr) {
            return Some((ptr.text_range(), *name));
        }
    }

    None
}

fn is_valid_ident(name: &str) -> bool {
    const KEYWORDS: &[&[u8]] = &[
        b"assert", b"else", b"if", b"in", b"inherit", b"let", b"or", b"rec", b"then", b"with",
    ];

    let bytes = name.as_bytes();
    !name.is_empty()
        && name.is_ascii()
        && (bytes[0].is_ascii_alphabetic() || bytes[0] == b'_')
        && bytes[1..]
            .iter()
            .all(|&b| b.is_ascii_alphanumeric() || b == b'_' || b == b'\'' || b == b'-')
        && !KEYWORDS.contains(&bytes)
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
                    format!("{}\n{}\n", src, text)
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
}
