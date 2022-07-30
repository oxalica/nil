use crate::FileRange;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Diagnostic {
    SyntaxError(FileRange, syntax::Error),
    InvalidDynamic(FileRange),
    DuplicatedKey(FileRange, FileRange),
}
