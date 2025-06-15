#[macro_use]
pub(crate) mod ty;

mod base;
mod def;
mod diagnostic;
mod ide;
mod text_edit;

#[cfg(test)]
mod tests;

pub use self::ide::{
    Analysis, AnalysisHost, Assist, AssistKind, Cancelled, CompletionItem, CompletionItemKind,
    GotoDefinitionResult, HlAttrField, HlKeyword, HlOperator, HlPunct, HlRange, HlRelated, HlTag,
    HoverResult, InlayHintKind, InlayHintResult, InlayHintsConfig, Link, LinkTarget,
    NavigationTarget, RenameResult, SymbolTree,
};
pub use base::{
    Change, FileId, FilePos, FileRange, FileSet, FlakeGraph, FlakeInfo, InFile, SourceDatabase,
    SourceRoot, SourceRootId, VfsPath,
};
pub use builtin::BuiltinKind;
pub use def::{DefDatabase, Module, ModuleKind, ModuleSourceMap, NameKind};
pub use diagnostic::{Diagnostic, DiagnosticKind, Severity};
pub use text_edit::{TextEdit, WorkspaceEdit};
pub use ty::{InferenceResult, TyDatabase};
