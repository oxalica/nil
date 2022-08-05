#[macro_use]
mod kind;

pub mod ast;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;

use core::fmt;

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
    MultipleRoots,
    UnexpectedToken,
    MultipleNoAssoc,
    MissingToken(SyntaxKind),
    MissingExpr,
    MissingAttr,
    PathTrailingSlash,
    PathDuplicatedSlashes,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MultipleRoots => "Multiple root expressions",
            Self::UnexpectedToken => "Unexpected token",
            Self::MultipleNoAssoc => "Invalid usage of no-associative operators",
            Self::MissingToken(tok) => return write!(f, "Missing {:?}", tok),
            Self::MissingExpr => "Missing expression",
            Self::MissingAttr => "Missing attribute",
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

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from(raw.0)
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
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
