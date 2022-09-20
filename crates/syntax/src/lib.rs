#[macro_use]
mod kind;

pub mod ast;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;

use core::fmt;

use rowan::TokenAtOffset;
pub use rowan::{self, TextRange, TextSize};

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
    MissingToken(SyntaxKind),
    MissingExpr,
    MissingElemExpr,
    MissingAttr,
    MissingParamIdent,
    MissingBinding,
    PathTrailingSlash,
    PathDuplicatedSlashes,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NestTooDeep => "Nest too deep",
            Self::MultipleRoots => "Multiple root expressions",
            Self::MultipleNoAssoc => "Invalid usage of no-associative operators",
            Self::MissingToken(tok) => return write!(f, "Missing {:?}", tok),
            Self::MissingExpr => "Missing expression",
            Self::MissingElemExpr => "Missing list element expression",
            Self::MissingAttr => "Missing attribute",
            Self::MissingParamIdent => "Missing parameter identifier",
            Self::MissingBinding => "Mising binding",
            Self::PathTrailingSlash => "Path has trailing slash",
            Self::PathDuplicatedSlashes => "Path has duplicated slashes",
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

pub type SyntaxNode = rowan::SyntaxNode<NixLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<NixLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<NixLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<NixLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<NixLanguage>;

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

/// Pick the most meaningful token at given cursor offset.
pub fn best_token_at_offset(node: &SyntaxNode, offset: TextSize) -> Option<SyntaxToken> {
    fn score(tok: SyntaxKind) -> u8 {
        match tok {
            SyntaxKind::ERROR | SyntaxKind::SPACE => 0,
            SyntaxKind::COMMENT => 1,
            SyntaxKind::PATH_START
            | SyntaxKind::PATH_END
            | SyntaxKind::PATH_FRAGMENT
            | SyntaxKind::STRING_FRAGMENT => 2,
            SyntaxKind::STRING_ESCAPE => 3,
            k if k.is_symbol() => 4,
            k if k.is_keyword() => 5,
            // IDENT, INT, and etc.
            _ => 6,
        }
    }

    match node.token_at_offset(offset) {
        TokenAtOffset::None => None,
        TokenAtOffset::Single(tok) => Some(tok),
        TokenAtOffset::Between(lhs, rhs) => {
            // Slightly prefer RHS.
            if score(lhs.kind()) > score(rhs.kind()) {
                Some(lhs)
            } else {
                Some(rhs)
            }
        }
    }
}
