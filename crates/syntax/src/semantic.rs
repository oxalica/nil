//! Auxiliary functions for semantics of AST nodes.
//! Mostly about syntax desugaring.
use crate::ast::{self, Attr, Expr, HasStringParts, StringPart};
use crate::lexer::KEYWORDS;
use crate::SyntaxNode;
use rowan::ast::{AstChildren, AstNode};
use std::borrow::Cow;
use std::str;

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
        return Cow::Borrowed(name);
    }
    Cow::Owned(
        std::iter::empty()
            .chain(Some('"'))
            .chain(name.chars().flat_map(|ch| ch.escape_default()))
            .chain(Some('"'))
            .collect(),
    )
}

/// Unescape a single string escape sequence.
///
/// The input should be from `StringPart::Escape` produced by the parser.
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

    let ret = n
        .string_parts()
        .fold(String::new(), |prev, part| match part {
            StringPart::Fragment(f) => prev + f.text(),
            StringPart::Escape(e) => prev + unescape_string_escape(e.text()),
            StringPart::Dynamic(_) => unreachable!(),
        });
    Some(ret)
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
