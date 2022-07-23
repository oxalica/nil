#[macro_use]
mod kind;

pub mod ast;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;

use core::fmt;

pub use rowan;
pub use rowan::{TextRange, TextSize};

pub use self::kind::SyntaxKind;
pub use self::parser::{parse_file, Parse};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Error {
    MultipleRoots,
    UnexpectedToken,
    MultipleNoAssoc,
    MissingToken(SyntaxKind),
    MissingExpr,
    MissingAttr,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MultipleRoots => "Multiple root expressions",
            Self::UnexpectedToken => "Unexpected token",
            Self::MultipleNoAssoc => "Invalid usage of no-associative operators",
            Self::MissingToken(tok) => return write!(f, "Missing {:?}", tok),
            Self::MissingExpr => "Missing expression",
            Self::MissingAttr => "Missing attribute",
        }
        .fmt(f)
    }
}

impl std::error::Error for Error {}

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
