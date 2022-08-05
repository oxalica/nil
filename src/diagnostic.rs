use std::fmt;
use syntax::{ErrorKind as SynErrorKind, TextRange};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Diagnostic {
    pub range: TextRange,
    pub kind: DiagnosticKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    SyntaxError(SynErrorKind),
    InvalidDynamic,
    DuplicatedKey,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    IncompleteSyntax,
}

impl Diagnostic {
    pub fn severity(&self) -> Severity {
        match self.kind {
            DiagnosticKind::SyntaxError(kind) => match kind {
                SynErrorKind::MultipleRoots
                | SynErrorKind::PathTrailingSlash
                | SynErrorKind::PathDuplicatedSlashes
                | SynErrorKind::MultipleNoAssoc => Severity::Error,
                SynErrorKind::UnexpectedToken
                | SynErrorKind::MissingToken(_)
                | SynErrorKind::MissingExpr
                | SynErrorKind::MissingAttr => Severity::IncompleteSyntax,
            },
            DiagnosticKind::InvalidDynamic | DiagnosticKind::DuplicatedKey => Severity::Error,
        }
    }

    pub fn message(&self) -> String {
        match self.kind {
            DiagnosticKind::SyntaxError(kind) => kind.to_string(),
            DiagnosticKind::InvalidDynamic => "Invalid location of dynamic attribute".into(),
            DiagnosticKind::DuplicatedKey => "Duplicated name definition".into(),
        }
    }
}

impl From<syntax::Error> for Diagnostic {
    fn from(err: syntax::Error) -> Self {
        Self {
            range: err.range,
            kind: DiagnosticKind::SyntaxError(err.kind),
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message(),
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )
    }
}
