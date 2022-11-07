mod completion;
mod diagnostics;
mod expand_selection;
mod goto_definition;
mod hover;
mod references;
mod rename;
mod symbol_hierarchy;
mod syntax_highlighting;

use crate::base::SourceDatabaseStorage;
use crate::def::DefDatabaseStorage;
use crate::ty::TyDatabaseStorage;
use crate::{Change, Diagnostic, FileId, FilePos, FileRange, WorkspaceEdit};
use rowan::TextRange;
use salsa::{Database, Durability, ParallelDatabase};
use smol_str::SmolStr;
use std::fmt;

pub use completion::{CompletionItem, CompletionItemKind};
pub use goto_definition::GotoDefinitionResult;
pub use hover::HoverResult;
pub use rename::RenameResult;
pub use symbol_hierarchy::SymbolTree;
pub use syntax_highlighting::{HlKeyword, HlOperator, HlPunct, HlRange, HlTag};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NavigationTarget {
    pub file_id: FileId,
    pub full_range: TextRange,
    pub focus_range: TextRange,
}

pub use salsa::Cancelled;

pub type Cancellable<T> = Result<T, Cancelled>;

#[salsa::database(SourceDatabaseStorage, DefDatabaseStorage, TyDatabaseStorage)]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(RootDatabase {
            storage: self.storage.snapshot(),
        })
    }
}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish_non_exhaustive()
    }
}

#[derive(Debug, Default)]
pub struct AnalysisHost {
    db: RootDatabase,
}

impl AnalysisHost {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn snapshot(&self) -> Analysis {
        Analysis {
            db: self.db.snapshot(),
        }
    }

    pub fn request_cancellation(&mut self) {
        self.db.salsa_runtime_mut().synthetic_write(Durability::LOW);
    }

    pub fn apply_change(&mut self, change: Change) {
        self.request_cancellation();
        change.apply(&mut self.db);
    }
}

#[derive(Debug)]
pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

impl Analysis {
    fn with_db<F, T>(&self, f: F) -> Cancellable<T>
    where
        F: FnOnce(&RootDatabase) -> T + std::panic::UnwindSafe,
    {
        Cancelled::catch(|| f(&self.db))
    }

    pub fn expand_selection(&self, frange: FileRange) -> Cancellable<Option<Vec<TextRange>>> {
        self.with_db(|db| expand_selection::expand_selection(db, frange))
    }

    pub fn syntax_highlight(
        &self,
        file: FileId,
        range: Option<TextRange>,
    ) -> Cancellable<Vec<HlRange>> {
        self.with_db(|db| syntax_highlighting::highlight(db, file, range))
    }

    pub fn diagnostics(&self, file: FileId) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| diagnostics::diagnostics(db, file))
    }

    pub fn goto_definition(&self, pos: FilePos) -> Cancellable<Option<GotoDefinitionResult>> {
        self.with_db(|db| goto_definition::goto_definition(db, pos))
    }

    pub fn completions(&self, pos: FilePos) -> Cancellable<Option<Vec<CompletionItem>>> {
        self.with_db(|db| completion::completions(db, pos))
    }

    pub fn references(&self, pos: FilePos) -> Cancellable<Option<Vec<FileRange>>> {
        self.with_db(|db| references::references(db, pos))
    }

    pub fn prepare_rename(&self, fpos: FilePos) -> Cancellable<RenameResult<(TextRange, SmolStr)>> {
        self.with_db(|db| rename::prepare_rename(db, fpos))
    }

    pub fn rename(
        &self,
        fpos: FilePos,
        new_name: &str,
    ) -> Cancellable<RenameResult<WorkspaceEdit>> {
        self.with_db(|db| rename::rename(db, fpos, new_name))
    }

    pub fn hover(&self, fpos: FilePos) -> Cancellable<Option<HoverResult>> {
        self.with_db(|db| hover::hover(db, fpos))
    }

    pub fn symbol_hierarchy(&self, file: FileId) -> Cancellable<Vec<SymbolTree>> {
        self.with_db(|db| symbol_hierarchy::symbol_hierarchy(db, file))
    }
}
