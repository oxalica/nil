use std::fmt;
use syntax::{ErrorKind as SynErrorKind, TextRange};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Diagnostic {
    pub range: TextRange,
    pub kind: DiagnosticKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    // Syntax.
    SyntaxError(SynErrorKind),

    // Lowering.
    InvalidDynamic,
    DuplicatedKey,
    EmptyInherit,
    LetAttrset,
    UriLiteral,
    MergePlainRecAttrset,
    MergeRecAttrset,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    IncompleteSyntax,
}

impl Diagnostic {
    pub fn severity(&self) -> Severity {
        match self.kind {
            DiagnosticKind::InvalidDynamic | DiagnosticKind::DuplicatedKey => Severity::Error,
            DiagnosticKind::EmptyInherit
            | DiagnosticKind::LetAttrset
            | DiagnosticKind::UriLiteral
            | DiagnosticKind::MergePlainRecAttrset
            | DiagnosticKind::MergeRecAttrset => Severity::Warning,
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
        }
    }

    pub fn message(&self) -> String {
        match self.kind {
            DiagnosticKind::SyntaxError(kind) => kind.to_string(),

            DiagnosticKind::InvalidDynamic => "Invalid location of dynamic attribute".into(),
            DiagnosticKind::DuplicatedKey => "Duplicated name definition".into(),
            DiagnosticKind::EmptyInherit => "Nothing inherited".into(),
            DiagnosticKind::LetAttrset => {
                "`let { ... }` is deprecated. Use `let ... in ...` instead".into()
            }
            DiagnosticKind::UriLiteral => {
                "URL literal is confusing and deprecated. Use strings instead".into()
            }
            DiagnosticKind::MergePlainRecAttrset => {
                "Merging non-rec-attrset with rec-attrset, the latter `rec` is implicitly ignored".into()
            }
            DiagnosticKind::MergeRecAttrset => {
                "Merging rec-attrset with other attrsets or attrpath. Merged values can unexpectedly reference each other remotely as in a single `rec { ... }`.".into()
            }
        }
    }

    pub fn is_unnecessary(&self) -> bool {
        matches!(self.kind, DiagnosticKind::EmptyInherit)
    }

    pub fn is_deprecated(&self) -> bool {
        matches!(
            self.kind,
            DiagnosticKind::LetAttrset | DiagnosticKind::UriLiteral
        )
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
