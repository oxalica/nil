//! Auxiliary functions for semantics of AST nodes.
//! Mostly about syntax desugaring.
use crate::ast::{Attr, Expr, HasStringParts, StringPart};
use std::str;

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

        if s.string_parts()
            .any(|part| matches!(part, StringPart::Dynamic(_)))
        {
            return Self::Dynamic(Some(Expr::String(s)));
        }

        let literal = s
            .string_parts()
            .fold(String::new(), |prev, part| match part {
                StringPart::Fragment(tok) => prev + tok.text(),
                StringPart::Escape(tok) => prev + unescape_string_escape(tok.text()),
                StringPart::Dynamic(_) => unreachable!(),
            });
        Self::Static(Some(literal))
    }
}
