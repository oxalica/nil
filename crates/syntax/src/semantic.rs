//! Auxiliary functions for semantics of AST nodes.
//! Mostly about syntax desugaring.
use crate::ast::{self, AstChildren, AstNode, Attr, Expr, HasStringParts, StringPart};
use crate::lexer::KEYWORDS;
use crate::{SyntaxNode, SyntaxToken};
use std::borrow::Cow;
use std::{fmt, str};

/// Check if a name is a valid identifier.
pub fn is_valid_ident(name: &str) -> bool {
    // This should match lexer impl.
    !name.is_empty()
        && matches!(name.as_bytes()[0], b'A'..=b'Z' | b'a'..=b'z' | b'_')
        && name
            .bytes()
            .all(|b| matches!(b, b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'0'..=b'9' | b'\'' | b'-'))
        && KEYWORDS.iter().all(|&(kw, _)| name != kw)
}

/// Escape a literal Attr. Quote it if it's not a valid identifier.
pub fn escape_literal_attr(name: &str) -> Cow<'_, str> {
    if is_valid_ident(name) {
        Cow::Borrowed(name)
    } else {
        Cow::Owned(escape_string(name))
    }
}

/// Escape the text in a string literal with double-quotes.
pub fn escape_string(text: &str) -> String {
    format!("\"{}\"", EscapeStringFragment(text))
}

#[derive(Debug, Clone)]
pub struct EscapeStringFragment<'a>(pub &'a str);

impl fmt::Display for EscapeStringFragment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, ch) in self.0.char_indices() {
            match ch {
                '"' => "\\\"",
                '\\' => "\\\\",
                '\n' => "\\n",
                '\r' => "\\r",
                '\t' => "\\r",
                '$' if self.0[i..].starts_with("${") => "\\$",
                _ => {
                    ch.fmt(f)?;
                    continue;
                }
            }
            .fmt(f)?;
        }
        Ok(())
    }
}

/// Unescape a single string escape sequence.
///
/// # Panics
/// The input must be from `StringPart::Escape` produced by the parser.
/// It will panic for unrecognized escape.
pub fn unescape_string_escape(escape: &str) -> &str {
    match escape {
        "''$" => "$",
        "'''" => "''",
        "''\\n" | "\\n" => "\n",
        "''\\r" | "\\r" => "\r",
        "''\\t" | "\\t" => "\t",
        _ if escape.starts_with('\\') => &escape[1..],
        _ if escape.starts_with("''\\") => &escape[3..],
        _ => panic!("Invalid escape sequence"),
    }
}

/// Retrieve the unescaped content of a String node if it contains no Dynamic.
/// Otherwise, return None.
pub fn unescape_string_literal(n: &ast::String) -> Option<String> {
    if n.string_parts()
        .any(|part| matches!(part, StringPart::Dynamic(_)))
    {
        return None;
    }
    let mut ret = String::new();
    unescape_string(n, |part| {
        match part {
            UnescapedStringPart::Fragment(frag) => ret += frag,
            UnescapedStringPart::Dynamic(_) => return Err(()),
        }
        Ok(())
    })
    .ok()
    .map(|()| ret)
}

/// Unescape strings and traverse the result parts.
pub fn unescape_string<E>(
    n: &ast::String,
    mut f: impl FnMut(UnescapedStringPart<'_>) -> Result<(), E>,
) -> Result<(), E> {
    for part in n.string_parts() {
        match part {
            StringPart::Fragment(tok) => f(UnescapedStringPart::Fragment(tok.text()))?,
            StringPart::Escape(tok) => f(UnescapedStringPart::Fragment(unescape_string_escape(
                tok.text(),
            )))?,
            StringPart::Dynamic(d) => f(UnescapedStringPart::Dynamic(d))?,
        }
    }
    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnescapedStringPart<'a> {
    Fragment(&'a str),
    Dynamic(ast::Dynamic),
}

/// Calculate the minimal indentation of an `IndentString`.
/// Or returns `usize::MAX` if all lines are empty.
///
/// See:
/// - <https://github.com/NixOS/nix/blob/2.11.0/src/libexpr/lexer.l#L204>
/// - <https://github.com/NixOS/nix/blob/2.11.0/src/libexpr/parser.y#L195>
pub fn common_indent_of(n: &ast::IndentString) -> usize {
    let mut ret = usize::MAX;
    let mut counter = Some(0usize);
    for part in n.string_parts() {
        let tok = match part {
            StringPart::Escape(_) | StringPart::Dynamic(_) => {
                if let Some(ind) = counter {
                    ret = ret.min(ind);
                    counter = None;
                }
                continue;
            }
            StringPart::Fragment(tok) => tok,
        };
        for b in tok.text().bytes() {
            match (b, &mut counter) {
                (b' ', Some(ind)) => *ind += 1,
                (b'\n', _) => counter = Some(0),
                (_, Some(ind)) => {
                    ret = ret.min(*ind);
                    counter = None;
                }
                _ => {}
            }
        }
    }

    // N.B. The last indentation doesn't count.
    ret
}

/// Strip the common indentation and traverse stripped parts.
/// See also [`common_indent_of`].
pub fn strip_indent<E>(
    n: &ast::IndentString,
    mut f: impl FnMut(StrippedStringPart<'_>) -> Result<(), E>,
) -> Result<(), E> {
    let indent = common_indent_of(n);

    let part_cnt = n.string_parts().count();
    for (i, part) in n.string_parts().enumerate() {
        let tok = match part {
            StringPart::Fragment(tok) => tok,
            StringPart::Escape(e) => {
                f(StrippedStringPart::Escape(e))?;
                continue;
            }
            StringPart::Dynamic(d) => {
                f(StrippedStringPart::Dynamic(d))?;
                continue;
            }
        };

        let mut frag = tok.text();
        // s/^ *\n//
        if i == 0 {
            if let Some(pos) = frag.find(|c| c != ' ') {
                if frag.as_bytes()[pos] == b'\n' {
                    frag = &frag[pos + 1..];
                }
            }
        }
        // s/\n *$/\n/
        if i + 1 == part_cnt {
            if let Some(pos) = frag.rfind(|c| c != ' ') {
                if frag.as_bytes()[pos] == b'\n' {
                    frag = &frag[..pos + 1];
                }
            }
        }
        for (j, line) in frag.split_inclusive('\n').enumerate() {
            if i != 0 && j == 0 {
                // Line continuation is kept.
                f(StrippedStringPart::Fragment(line))?;
            } else {
                let line_len = line.strip_suffix('\n').unwrap_or(line).len();
                f(StrippedStringPart::Fragment(&line[indent.min(line_len)..]))?;
            }
        }
    }

    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StrippedStringPart<'a> {
    Fragment(&'a str),
    Escape(SyntaxToken),
    Dynamic(ast::Dynamic),
}

/// The dynamic-ness of an `Attr`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrKind {
    Static(Option<String>),
    Dynamic(Option<Expr>),
}

impl AttrKind {
    /// Classify the dynamic-ness of an `Attr`, by
    /// unwrapping nested parentheses and extracting string literals.
    pub fn of(attr: Attr) -> Self {
        let s = match attr {
            Attr::Name(n) => return Self::Static(n.token().map(|tok| tok.text().into())),
            Attr::String(s) => s,
            Attr::Dynamic(d) => match d.expr().and_then(Expr::flatten_paren) {
                Some(Expr::String(s)) => s,
                e => return Self::Dynamic(e),
            },
        };

        match unescape_string_literal(&s) {
            Some(lit) => Self::Static(Some(lit)),
            None => Self::Dynamic(Some(Expr::String(s))),
        }
    }
}

pub trait HasBindingsDesugar {
    type IntoIter: Iterator<Item = BindingDesugar>;

    fn expr_syntax(&self) -> Option<&SyntaxNode>;

    fn desugar_bindings(&self) -> Self::IntoIter;
}

impl<N: ast::HasBindings> HasBindingsDesugar for N {
    type IntoIter = std::iter::Map<AstChildren<ast::Binding>, fn(ast::Binding) -> BindingDesugar>;

    fn expr_syntax(&self) -> Option<&SyntaxNode> {
        Some(<N as AstNode>::syntax(self))
    }

    fn desugar_bindings(&self) -> Self::IntoIter {
        self.bindings().map(BindingDesugar::of)
    }
}

#[derive(Debug, Clone)]
pub enum BindingDesugar {
    Inherit(ast::Inherit),
    AttrValue(Option<ast::Attr>, BindingValueKind),
}

impl BindingDesugar {
    fn of(b: ast::Binding) -> Self {
        let pv = match b {
            ast::Binding::Inherit(i) => return Self::Inherit(i),
            ast::Binding::AttrpathValue(pv) => pv,
        };
        let value = pv.value();
        let (attr, value_kind) = match pv.attrpath() {
            None => (None, BindingValueKind::of(None, value)),
            Some(path) => {
                let mut iter = path.attrs();
                (iter.next(), BindingValueKind::of(Some(iter), value))
            }
        };
        Self::AttrValue(attr, value_kind)
    }
}

#[derive(Debug, Clone)]
pub enum BindingValueKind {
    Expr(Option<ast::Expr>),
    ImplicitSet(ImplicitSet),
    ExplicitSet(ast::AttrSet),
}

impl BindingValueKind {
    fn of(attr_iter: Option<AstChildren<Attr>>, value: Option<ast::Expr>) -> Self {
        if let Some(mut attr_iter) = attr_iter {
            if let Some(attr) = attr_iter.next() {
                return Self::ImplicitSet(ImplicitSet {
                    attr,
                    attr_iter,
                    value,
                });
            }
        }
        match value.and_then(|e| e.flatten_paren()) {
            Some(ast::Expr::AttrSet(set)) if set.let_token().is_none() => Self::ExplicitSet(set),
            value => Self::Expr(value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImplicitSet {
    attr: ast::Attr,
    attr_iter: AstChildren<ast::Attr>,
    value: Option<ast::Expr>,
}

impl HasBindingsDesugar for ImplicitSet {
    type IntoIter = ImplicitSetIter;

    fn expr_syntax(&self) -> Option<&SyntaxNode> {
        None
    }

    fn desugar_bindings(&self) -> Self::IntoIter {
        ImplicitSetIter(Some(self.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct ImplicitSetIter(Option<ImplicitSet>);

impl Iterator for ImplicitSetIter {
    type Item = BindingDesugar;

    fn next(&mut self) -> Option<Self::Item> {
        let ImplicitSet {
            attr,
            mut attr_iter,
            value,
        } = self.0.take()?;
        let value_kind = match attr_iter.next() {
            None => BindingValueKind::of(Some(attr_iter), value),
            Some(next_attr) => BindingValueKind::ImplicitSet(ImplicitSet {
                attr: next_attr,
                attr_iter,
                value,
            }),
        };
        Some(BindingDesugar::AttrValue(Some(attr), value_kind))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::parse;

    #[test]
    fn ident_validity() {
        assert!(is_valid_ident("foo"));
        assert!(is_valid_ident("bar'-"));
        assert!(!is_valid_ident(" "));
        assert!(!is_valid_ident("a*b"));
        assert!(!is_valid_ident("in"));
    }

    #[test]
    fn escape_attr() {
        assert_eq!(escape_literal_attr("foo"), "foo");
        assert_eq!(escape_literal_attr("in"), r#""in""#);
        assert_eq!(escape_literal_attr(" "), r#"" ""#);
        assert_eq!(escape_literal_attr("\n"), r#""\n""#);
        assert_eq!(escape_literal_attr("$ ${"), r#""$ \${""#);
    }

    #[test]
    fn escape_string_() {
        assert_eq!(escape_string(""), r#""""#);
        assert_eq!(escape_string("n\"$a \n${b"), r#""n\"$a \n\${b""#);
    }

    #[test]
    fn unescape_string_literal() {
        let unescape = |src| super::unescape_string_literal(&parse(src));
        assert_eq!(unescape(r#""foo\n""#), Some("foo\n".into()));
        assert_eq!(unescape(r#""foo\n${"b"}""#), None);
    }

    #[test]
    fn unescape_string_parts() {
        let unescape = |src| {
            let mut ret = String::new();
            unescape_string::<()>(&parse(src), |part| {
                match part {
                    UnescapedStringPart::Fragment(frag) => ret += frag,
                    UnescapedStringPart::Dynamic(_) => ret += "<dyn>",
                }
                Ok(())
            })
            .unwrap();
            ret
        };
        assert_eq!(unescape(r#""foo\n""#), "foo\n");
        assert_eq!(unescape(r#""foo\n${"b"}""#), "foo\n<dyn>");
    }

    #[test]
    fn strip_indent_string() {
        #[track_caller]
        fn check(src: &str, expect_indent: usize, expect: &str) {
            let s = parse(src);
            assert_eq!(common_indent_of(&s), expect_indent);
            let mut stripped = String::new();
            strip_indent::<()>(&s, |part| {
                match part {
                    StrippedStringPart::Fragment(frag) => stripped += frag,
                    StrippedStringPart::Escape(e) => stripped += unescape_string_escape(e.text()),
                    StrippedStringPart::Dynamic(_) => stripped += "<dyn>",
                }
                Ok(())
            })
            .unwrap();
            assert_eq!(stripped, expect);
        }

        // Trivial cases.
        check("'' a ''", 1, "a ");
        check("''a\n a''", 0, "a\n a");
        check("''''\\n\n a''", 0, "\n\n a");
        check("''${a}\n a''", 0, "<dyn>\n a");

        // The first empty line is stripped.
        check("'' \nb''", 0, "b");
        check("'' a\nb''", 0, " a\nb");
        check("''\n  a\n  b''", 2, "a\nb");
        check("'' \n  a\n  b''", 2, "a\nb");

        // The last line is stripped if it's empty.
        check("''\n  a\n ''", 2, "a\n");
        check("''\n  a\n     ''", 2, "a\n");
        check("''\n  a ''", 2, "a ");
        check("''\n  a \n  b ''", 2, "a \nb ");

        // Not indentation.
        check("''\n  a b ${c} \n  d ''' ''", 2, "a b <dyn> \nd '' ");

        // Minimal.
        check("'' ''", usize::MAX, "");
        check("'' a''", 1, "a");
        check("''\n a\n  b''", 1, "a\n b");
        check("''\n  a\n b''", 1, " a\nb");
        check("''\n a\n b''", 1, "a\nb");
        check("''\n a\n  b\n  ''", 1, "a\n b\n");
        check("''\n a\n  b\n  c ''", 1, "a\n b\n c ");

        check(
            "
''
   a
  b
    c
''",
            2,
            " a\nb\n  c\n",
        );
        check(
            "
  ''
    a
   ''
        ",
            4,
            "a\n",
        );
    }

    #[test]
    fn attr_kind() {
        let classify = |src| match AttrKind::of(parse(src)) {
            AttrKind::Static(text) => Ok(text.unwrap()),
            AttrKind::Dynamic(expr) => Err(expr.unwrap().syntax().to_string()),
        };

        assert_eq!(classify("{ a-b = 1; }"), Ok("a-b".into()));
        assert_eq!(classify(r#"{ "a\n" = 1; }"#), Ok("a\n".into()));
        assert_eq!(classify(r#"{ ${"b"} = 1; }"#), Ok("b".into()));
        assert_eq!(classify(r#"{ ${(("b"))} = 1; }"#), Ok("b".into()));

        assert_eq!(
            classify(r#"{ "a${"b"}" = 1; }"#),
            Err(r#""a${"b"}""#.into()),
        );
        assert_eq!(
            classify(r#"{ ${"b" + "c"} = 1; }"#),
            Err(r#""b" + "c""#.into()),
        );
    }

    #[test]
    fn desugar_bindings_flat() {
        let e = parse::<ast::AttrSet>("{ a = 1; ${b} = 2; inherit c; }");
        assert!(e.expr_syntax().is_some());

        let mut iter = e.desugar_bindings();

        assert!(matches!(iter.next().unwrap(),
            BindingDesugar::AttrValue(Some(k), BindingValueKind::Expr(Some(v)))
            if k.syntax().to_string() == "a" && v.syntax().to_string() == "1"
        ));
        assert!(matches!(iter.next().unwrap(),
            BindingDesugar::AttrValue(Some(k), BindingValueKind::Expr(Some(v)))
            if k.syntax().to_string() == "${b}" && v.syntax().to_string() == "2"
        ));
        assert!(matches!(iter.next().unwrap(),
            BindingDesugar::Inherit(i)
            if i.syntax().to_string() == "inherit c;"
        ));

        assert!(iter.next().is_none());
    }

    #[test]
    fn desugar_bindings_nest() {
        let e = parse::<ast::AttrSet>("{ a.b = 1; c = rec { d = 2; }; }");
        assert!(e.expr_syntax().is_some());

        let mut iter = e.desugar_bindings();

        match iter.next().unwrap() {
            BindingDesugar::AttrValue(Some(k), BindingValueKind::ImplicitSet(set)) => {
                assert_eq!(k.syntax().to_string(), "a");
                assert!(set.expr_syntax().is_none());
                let mut deep = set.desugar_bindings();
                assert!(matches!(deep.next().unwrap(),
                    BindingDesugar::AttrValue(Some(k), BindingValueKind::Expr(Some(v)))
                    if k.syntax().to_string() == "b" && v.syntax().to_string() == "1"
                ));
                assert!(deep.next().is_none());
            }
            _ => unreachable!(),
        }

        match iter.next().unwrap() {
            BindingDesugar::AttrValue(Some(k), BindingValueKind::ExplicitSet(set)) => {
                assert_eq!(k.syntax().to_string(), "c");
                assert_eq!(set.syntax().to_string(), "rec { d = 2; }");
                let mut deep = set.desugar_bindings();
                assert!(matches!(deep.next().unwrap(),
                    BindingDesugar::AttrValue(Some(k), BindingValueKind::Expr(Some(v)))
                    if k.syntax().to_string() == "d" && v.syntax().to_string() == "2"
                ));
                assert!(deep.next().is_none());
            }
            _ => unreachable!(),
        }

        assert!(iter.next().is_none());
    }
}
