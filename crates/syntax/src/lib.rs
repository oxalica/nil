#[macro_use]
mod kind;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod semantic;

#[cfg(test)]
mod tests;

use core::fmt;
use rowan::TokenAtOffset;

pub use rowan::{self, NodeOrToken, TextRange, TextSize};
pub type SyntaxNode = rowan::SyntaxNode<NixLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<NixLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<NixLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<NixLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<NixLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<NixLanguage>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<NixLanguage>;

pub use self::kind::SyntaxKind;
pub use self::parser::{parse_file, Parse};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Error {
    pub range: TextRange,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    NestTooDeep,
    MultipleRoots,
    MultipleNoAssoc,
    ExpectToken(SyntaxKind),
    ExpectExpr,
    ExpectElemExpr,
    ExpectAttr,
    ExpectIdent,
    ExpectBinding,
    PathTrailingSlash,
    PathDuplicatedSlashes,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NestTooDeep => "Nest too deep",
            Self::MultipleRoots => "Multiple top-level expressions are not allowed",
            Self::MultipleNoAssoc => "No-associative operators cannot be chained",
            Self::ExpectToken(tok) => return write!(f, "Expecting {tok}"),
            Self::ExpectExpr => "Expecting an expression",
            Self::ExpectElemExpr => "Expecting a list element expression. Forget parentheses?",
            Self::ExpectAttr => {
                r#"Expecting an attribute like `name`, `"name"` or `${expression}`"#
            }
            Self::ExpectIdent => "Expecting an identifier like `name`",
            Self::ExpectBinding => "Expecting a binding like `path = value;` or `inherit attr;`",
            Self::PathTrailingSlash => "Path with trailing slash is not allowed",
            Self::PathDuplicatedSlashes => "Path with duplicated slashes is not allowed",
        }
        .fmt(f)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.kind,
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )
    }
}

impl std::error::Error for ErrorKind {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NixLanguage {}

impl rowan::Language for NixLanguage {
    type Kind = SyntaxKind;

    #[inline(always)]
    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        raw.into()
    }

    #[inline(always)]
    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// Matches a `SyntaxNode` against an `ast` type.
///
/// # Example:
///
/// ```
/// # use syntax::{SyntaxNode, match_ast, ast};
/// # fn main() {
/// # let node: SyntaxNode = return;
/// match_ast! {
///     match node {
///         ast::AttrpathValue(it) => {},
///         ast::PatField(it) => {},
///         _ => {},
///     }
/// }
/// # }
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => {
        match_ast!(match ($node) { $($tt)* })
    };
    (match ($node:expr) {
        $( $( $path:ident )::+ ($it:pat) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = <$($path)::* as $crate::rowan::ast::AstNode>::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}

/// Pick the most likely interested token at given cursor offset.
pub fn best_token_at_offset(node: &SyntaxNode, offset: TextSize) -> Option<SyntaxToken> {
    fn score(tok: SyntaxKind) -> u8 {
        match tok {
            SyntaxKind::ERROR | SyntaxKind::SPACE => 0,
            SyntaxKind::COMMENT => 1,

            // Avoid returning sibling delimiters, which may be in another region.
            // Sorted by the nesting level in AST.
            // `{ ...;|b = ... }`
            T![;] | T![,] => 2,
            T![=] | T![:] | T![@] => 3,
            T!['('] | T!['['] | T!['{'] | T!["${"] | T![')'] | T![']'] | T!['}'] => 4,

            // The rest are mostly operators, including `.` and `?`.
            k if k.is_punct() => 5,

            // Literal parts.
            T!['"'] | T!["''"] => 10,
            SyntaxKind::PATH_START
            | SyntaxKind::PATH_END
            | SyntaxKind::PATH_FRAGMENT
            | SyntaxKind::STRING_FRAGMENT => 11,
            SyntaxKind::STRING_ESCAPE => 12,

            // Atoms.
            k if k.is_keyword() => 13,
            // IDENT, INT, and etc.
            _ => 14,
        }
    }

    match node.token_at_offset(offset) {
        TokenAtOffset::None => None,
        TokenAtOffset::Single(tok) => Some(tok),
        // Slightly prefer RHS for equal scores.
        TokenAtOffset::Between(lhs, rhs) if score(lhs.kind()) > score(rhs.kind()) => Some(lhs),
        TokenAtOffset::Between(_, rhs) => Some(rhs),
    }
}
