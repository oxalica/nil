mod completion;
mod diagnostics;
mod goto_definition;
mod references;

use crate::base::SourceDatabaseStorage;
use crate::def::DefDatabaseStorage;
use crate::{Change, Diagnostic, FileId, FilePos, FileRange};
use rowan::TextRange;
use salsa::{Cancelled, Database, Durability, ParallelDatabase};
use std::fmt;

pub use completion::{CompletionItem, CompletionItemKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NavigationTarget {
    pub file_id: FileId,
    pub full_range: TextRange,
    pub focus_range: TextRange,
}

pub type Cancellable<T> = Result<T, Cancelled>;

#[salsa::database(SourceDatabaseStorage, DefDatabaseStorage)]
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

    pub fn diagnostics(&self, file: FileId) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| diagnostics::diagnostics(db, file))
    }

    pub fn goto_definition(&self, pos: FilePos) -> Cancellable<Option<Vec<NavigationTarget>>> {
        self.with_db(|db| goto_definition::goto_definition(db, pos))
    }

    pub fn completions(&self, pos: FilePos) -> Cancellable<Option<Vec<CompletionItem>>> {
        self.with_db(|db| completion::completions(db, pos))
    }

    pub fn references(&self, pos: FilePos) -> Cancellable<Option<Vec<FileRange>>> {
        self.with_db(|db| references::references(db, pos))
    }
}
