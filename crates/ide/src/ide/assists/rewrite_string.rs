//! Rewrite between different types of strings
//! - URI literals -> Double quoted strings
//! - Attribute names <-> Double quoted strings
//! - Double quoted strings <-> Indented strings
use std::convert::Infallible;
use std::fmt::Write;

use super::{AssistKind, AssistsCtx};
use crate::TextEdit;
use syntax::ast::{self, AstNode};
use syntax::semantic::{
    is_valid_ident, strip_indent, unescape_string, unescape_string_escape, unescape_string_literal,
    EscapeStringFragment, StrippedStringPart, UnescapedStringPart,
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
    let token = ctx
        .covering_node::<ast::Literal>()
        .filter(|lit| lit.kind() == Some(ast::LiteralKind::Uri))?
        .token()?;

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
pub(super) fn unquote_attr(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::String>()?;
    let syntax = node.syntax();
    if syntax
        .parent()
        .map_or(true, |parent| parent.kind() != SyntaxKind::ATTR_PATH)
    {
        return None;
    };

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
}

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
pub(super) fn rewrite_string_to_indented(ctx: &mut AssistsCtx<'_>) -> Option<()> {
    let node = ctx.covering_node::<ast::String>()?;
    let syntax = node.syntax();
    if syntax
        .parent()
        .map_or(false, |parent| parent.kind() == SyntaxKind::ATTR_PATH)
    {
        return None;
    }

    let start = node.start_dquote_token()?.text_range().start().into();
    let file = &*ctx.db.file_content(ctx.frange.file_id);
    // The current line until the start of the string.
    let line = file[..start].rsplit_once('\n').map_or(file, |(_, x)| x);
    // Indentation of the current line.
    let indent = line
        .split_once(|c: char| !c.is_whitespace())
        .map_or(line, |(indent, _)| indent);
    let indent = format!("{indent}  ");

    let mut text = format!("''\n{indent}");
    let mut line_start = true;
    let _ = unescape_string::<Infallible>(&node, |part| {
        match part {
            UnescapedStringPart::Fragment(frag) => {
                if frag.is_empty() {
                    return Ok(());
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
                            if line_start {
                                text.insert(text.len() - indent.len(), '\n');
                            } else {
                                text.push('\n');
                                text.push_str(&indent);
                                line_start = true;
                                continue;
                            }
                        }
                        _ => text.push(x),
                    }
                    line_start = false;
                }
            }

            UnescapedStringPart::Dynamic(dyna) => {
                line_start = false;
                text.push_str(&dyna.syntax().to_string());
            }
        };

        Ok(())
    });
    if line_start {
        text.truncate(text.len() - 2);
    }
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
    let mut ret = String::from('"');

    // Concatenate all contiguous fragments and escape them in a single run.
    // This correctly handles `${` on two individual fragments/escapes.
    // Eg. `''${` => [Escape("$"), Fragment("{")] => "${" => "\${"
    // Note that either part alone doesn't require escaping.
    let mut last_frag = String::new();
    strip_indent::<Infallible>(&node, |part| {
        match part {
            StrippedStringPart::Fragment(frag) => {
                last_frag += frag;
            }
            StrippedStringPart::Escape(esc) => {
                last_frag += unescape_string_escape(esc.text());
            }
            StrippedStringPart::Dynamic(dyna) => {
                write!(ret, "{}", EscapeStringFragment(&last_frag)).unwrap();
                last_frag.clear();
                ret += &dyna.syntax().to_string();
            }
        }
        Ok(())
    })
    .unwrap();
    write!(ret, "{}", EscapeStringFragment(&last_frag)).unwrap();
    ret.push('"');

    ctx.add(
        "rewrite_indented_to_string",
        "Rewrite the indented string to a double quoted string",
        AssistKind::RefactorRewrite,
        vec![TextEdit {
            delete: node.syntax().text_range(),
            insert: ret.into(),
        }],
    );

    Some(())
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
        define_check_assist!(super::unquote_attr);

        check(r#"{ $0"foo" = bar; }"#, expect!["{ foo = bar; }"]);
        check(r#"{ foo.$0"bar" = baz; }"#, expect!["{ foo.bar = baz; }"]);
        check(
            r#"let $0"foo".bar = baz; in foo"#,
            expect!["let foo.bar = baz; in foo"],
        );

        check_no(r#"$0"foo""#);
        check_no(r#"{ ${fo$0o} = bar; }"#);
        check_no(r#"{ $0"foo\n" = bar; }"#);
        check_no(r#"{ "foo.$0bar" = baz; }"#);
    }

    #[test]
    fn string_to_indented() {
        define_check_assist!(super::rewrite_string_to_indented);

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

        check_no(r#"{ $0"foo" = bar; }"#);
    }

    #[test]
    fn indented_to_string() {
        define_check_assist!(super::rewrite_indented_to_string);

        check("$0''\n foo''", expect![r#""foo""#]);
        check("''\n foo\n  $0bar\n''", expect![r#""foo\n bar\n""#]);
        check(r"$0''\n\r\t''", expect![r#""\\n\\r\\t""#]);
        check(r"'''''''$0", expect![r#""''""#]);
        check(r"$0''''${foo}''", expect![r#""\${foo}""#]);

        // See comments in `rewrite_indented_to_string`.
        check(r"$0'' ''${ ''", expect![[r#""\${ ""#]]);
        check(r"$0'' ''$ ''", expect![[r#""$ ""#]]);
    }
}
