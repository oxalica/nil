use crate::FileId;
use crate::{base::SourceDatabaseStorage, def::DefDatabaseStorage};
use rowan::TextRange;
use std::fmt;

mod goto_definition;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NavigationTarget {
    pub file_id: FileId,
    pub full_range: TextRange,
    pub focus_range: TextRange,
}

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
