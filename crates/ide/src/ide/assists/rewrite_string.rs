//! Rewrite between different types of strings
//! - URI literals -> Double quoted strings
//! - Attribute names <-> Double quoted strings
//! - Double quoted strings <-> Indented strings
use std::convert::Infallible;

use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use syntax::ast::{self, AstNode};
use syntax::semantic::{
    is_valid_ident, strip_indent, unescape_string, unescape_string_escape, unescape_string_literal,
    StrippedStringPart, UnescapedStringPart,
};
use syntax::SyntaxKind;

/// URI literal -> Double quoted strings
/// ```nix
/// https://nixos.org
/// ```
/// =>
/// ```nix
/// "https://nixos.org"
/// ```
pub(super) fn rewrite_uri_to_string(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::Literal>()?;
    // Node should be a URI literal
    if node.kind()? != ast::LiteralKind::Uri {
        return None;
    }

    let token = node.token()?;
    ctx.add(
        "rewrite_uri_to_string",
        "Rewrite the URI literal to a double quoted string",
        AssistKind::QuickFix,
        vec![TextEdit {
            delete: token.text_range(),
            insert: format!(r#""{}""#, token.text()).into(),
        }],
    );

    Some(())
}

/// Attribute names -> Double quoted strings
/// ```nix
/// { foo = bar; }
/// ```
/// =>
/// ```nix
/// { "foo" = bar; }
/// ```
pub(super) fn quote_attr(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::Name>()?;
    let token = node.token()?;
    let syntax = node.syntax();

    // Name should be an attribute, not a e.g. lambda parameter.
    if syntax.parent().map_or(true, |parent| {
        !matches!(parent.kind(), SyntaxKind::ATTR_PATH | SyntaxKind::INHERIT)
    }) {
        return None;
    }

    ctx.add(
        "quote_attr",
        "Put the attribute name in double quotes",
        AssistKind::RefactorRewrite,
        vec![TextEdit {
            delete: node.syntax().text_range(),
            insert: format!(r#""{}""#, token.text()).into(),
        }],
    );
    Some(())
}

/// Double quoted attributes name -> Attribute names without quotes
/// ```nix
/// { "foo" = bar; }
/// ```
/// =>
/// ```nix
/// { foo = bar; }
/// ```
///
/// Double quoted strings -> Indented strings
/// ```nix
/// "foo\nbar\n"
/// ```
/// =>
/// ```nix
/// ''
///   foo
///   bar
/// ''
/// ```
pub(super) fn rewrite_string(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::String>()?;
    let syntax = node.syntax();
    if syntax
        .parent()
        .map_or(false, |parent| parent.kind() == SyntaxKind::ATTR_PATH)
    {
        let text = unescape_string_literal(&node)?;
        if is_valid_ident(&text) {
            ctx.add(
                "unquote_attr",
                "Remove the double quotes from the attribute name",
                AssistKind::RefactorRewrite,
                vec![TextEdit {
                    delete: node.syntax().text_range(),
                    insert: text.into(),
                }],
            );
        }
        Some(())
    } else {
        let start = node.start_dquote_token()?.text_range().start().into();
        let file = &*ctx.db.file_content(ctx.frange.file_id);
        // The current line until the start of the string.
        let line = file[..start].rsplit_once('\n').map_or(file, |(_, x)| x);
        // Indentation of the current line.
        let indent = line
            .split_once(|c: char| !c.is_whitespace())
            .map_or(line, |(indent, _)| indent);

        let mut text = format!("''\n{indent}");
        let mut line_start = true;
        let _ = unescape_string::<Infallible>(&node, |part| {
            match part {
                UnescapedStringPart::Fragment(frag) => {
                    if frag.is_empty() {
                        return Ok(());
                    }
                    if line_start {
                        text.push_str("  ");
                        line_start = false;
                    }

                    let mut chars = frag.chars();
                    while let Some(x) = chars.next() {
                        match x {
                            '$' => match chars.next() {
                                Some('{') => text.push_str("''${"),
                                Some(y) => {
                                    text.push('$');
                                    text.push(y);
                                }
                                // It's impossible to know from this context whether the next
                                // character is '{' or not, so we assume it is just to be safe
                                None => text.push_str("''$"),
                            },
                            '\'' => match chars.next() {
                                Some('\'') => text.push_str("'''"),
                                Some(y) => {
                                    text.push('\'');
                                    text.push(y);
                                }
                                None => text.push('\''),
                            },
                            '\n' => {
                                text.push('\n');
                                text.push_str(indent);
                                line_start = true;
                            }
                            _ => text.push(x),
                        }
                    }
                }

                UnescapedStringPart::Dynamic(dyna) => {
                    if line_start {
                        text.push_str("  ");
                        line_start = false;
                    }
                    text.push_str(&dyna.syntax().to_string());
                }
            };

            Ok(())
        });
        text.push_str("''");

        ctx.add(
            "rewrite_string_to_indented",
            "Rewrite the double quoted string to an indented string",
            AssistKind::RefactorRewrite,
            vec![TextEdit {
                delete: node.syntax().text_range(),
                insert: text.into(),
            }],
        );

        Some(())
    }
}

/// Indented strings -> Double quoted strings
/// ```nix
/// ''
///   foo
///   bar
/// ''
/// =>
/// ```nix
/// "foo\nbar\n"
/// ```
pub(super) fn rewrite_indented_to_string(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::IndentString>()?;
    let mut text = String::from('"');
    let _ = strip_indent::<Infallible>(&node, |part| {
        match part {
            StrippedStringPart::Fragment(frag) => {
                escape_dquote_string(&mut text, frag);
            }
            StrippedStringPart::Escape(esc) => {
                escape_dquote_string(&mut text, unescape_string_escape(esc.text()));
            }
            StrippedStringPart::Dynamic(dyna) => {
                text += &dyna.syntax().to_string();
            }
        }
        Ok(())
    });
    text.push('"');

    ctx.add(
        "rewrite_indented_to_string",
        "Rewrite the indented string to a double quoted string",
        AssistKind::RefactorRewrite,
        vec![TextEdit {
            delete: node.syntax().text_range(),
            insert: text.into(),
        }],
    );

    Some(())
}

/// Escape `text` and write to `out`,
/// `text` may be only a fragment of the original Nix string.
fn escape_dquote_string(out: &mut String, text: &str) {
    let mut xs = text.chars();
    while let Some(x) = xs.next() {
        match x {
            '"' | '\\' => {
                out.push('\\');
                out.push(x);
            }
            '$' => match xs.next() {
                Some('{') => out.push_str("\\${"),
                Some(y) => {
                    out.push('$');
                    out.push(y);
                }
                // It's impossible to know from this context whether the next
                // character is '{' or not, so we assume it is just to be safe
                None => out.push_str("\\$"),
            },
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(x),
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    #[test]
    fn uri() {
        define_check_assist!(super::rewrite_uri_to_string);

        check("$0https://nixos.org", expect![r#""https://nixos.org""#]);
        check(
            "https://github.com/oxalica/nil$0",
            expect![r#""https://github.com/oxalica/nil""#],
        );
    }

    #[test]
    fn quote_attr() {
        define_check_assist!(super::quote_attr);

        check("{ $0foo = bar; }", expect![r#"{ "foo" = bar; }"#]);
        check("{ foo.$0bar = baz; }", expect![r#"{ foo."bar" = baz; }"#]);
        check(
            "{ inherit (foo) $0bar; }",
            expect![r#"{ inherit (foo) "bar"; }"#],
        );
        check(
            "let f$0oo.bar = baz; in foo",
            expect![r#"let "foo".bar = baz; in foo"#],
        );

        check_no("{ foo$0 }: foo");
        check_no("{ inherit (foo$0) bar; }");
    }

    #[test]
    fn unquote_attr() {
        define_check_assist!(super::rewrite_string);

        check(r#"{ $0"foo" = bar; }"#, expect!["{ foo = bar; }"]);
        check(r#"{ foo.$0"bar" = baz; }"#, expect!["{ foo.bar = baz; }"]);
        check(
            r#"let $0"foo".bar = baz; in foo"#,
            expect!["let foo.bar = baz; in foo"],
        );

        check_no(r#"{ ${fo$0o} = bar; }"#);
        check_no(r#"{ $0"foo\n" = bar; }"#);
        check_no(r#"{ "foo.$0bar" = baz; }"#);
    }

    #[test]
    fn string_to_indented() {
        define_check_assist!(super::rewrite_string);

        check(r#"$0"foo""#, expect!["''\n  foo''\n"]);
        check(r#""''"$0"#, expect!["''\n  '''''\n"]);
        check(r#"$0"${foo}""#, expect!["''\n  ${foo}''\n"]);
        check(r#"$0"\${foo}""#, expect!["''\n  ''${foo}''\n"]);
        check("foo\n  bar \"$0\"", expect!["foo\n  bar ''\n  ''\n"]);
        check(
            "foo\n  $0\"bar\\n\\n  baz\\n\"",
            expect![
                "
                    foo
                      ''
                        bar

                          baz
                      ''
                "
            ],
        );
    }

    #[test]
    fn indented_to_string() {
        define_check_assist!(super::rewrite_indented_to_string);

        check("$0''\n foo''", expect![r#""foo""#]);
        check("''\n foo\n  $0bar\n''", expect![r#""foo\n bar\n""#]);
        check(r"$0''\n\r\t''", expect![r#""\\n\\r\\t""#]);
        check(r"'''''''$0", expect![r#""''""#]);
        check(r"$0''''${foo}''", expect![r#""\${foo}""#]);
    }
}
