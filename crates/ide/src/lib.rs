mod base;
mod def;
mod diagnostic;
mod ide;
mod text_edit;
mod ty;

#[cfg(test)]
mod tests;

pub use self::ide::{
    Analysis, AnalysisHost, Cancelled, CompletionItem, CompletionItemKind, GotoDefinitionResult,
    HlKeyword, HlOperator, HlPunct, HlRange, HlTag, HoverResult, Link, LinkTarget,
    NavigationTarget, RenameResult, RootDatabase, SymbolTree,
};
pub use base::{
    Change, FileId, FilePos, FileRange, FileSet, InFile, SourceDatabase, SourceRoot, SourceRootId,
    VfsPath,
};
pub use builtin::BuiltinKind;
pub use def::{DefDatabase, Module, ModuleSourceMap, NameKind};
pub use diagnostic::{Diagnostic, DiagnosticKind, Severity};
pub use text_edit::{TextEdit, WorkspaceEdit};
pub use ty::{InferenceResult, TyDatabase};
