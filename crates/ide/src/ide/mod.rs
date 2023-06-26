mod assists;
mod completion;
mod diagnostics;
mod expand_selection;
mod file_references;
mod goto_definition;
mod highlight_related;
mod hover;
mod links;
mod references;
mod rename;
mod symbol_hierarchy;
mod syntax_highlighting;

use crate::base::SourceDatabaseStorage;
use crate::def::DefDatabaseStorage;
use crate::ty::TyDatabaseStorage;
use crate::{
    Change, Diagnostic, FileId, FilePos, FileRange, FileSet, SourceRoot, VfsPath, WorkspaceEdit,
};
use nix_interop::DEFAULT_IMPORT_FILE;
use salsa::{Database, Durability, ParallelDatabase};
use smol_str::SmolStr;
use std::fmt;
use std::sync::Arc;
use syntax::TextRange;

pub use assists::{Assist, AssistKind};
pub use completion::{CompletionItem, CompletionItemKind};
pub use goto_definition::GotoDefinitionResult;
pub use highlight_related::HlRelated;
pub use hover::HoverResult;
pub use links::{Link, LinkTarget};
pub use rename::RenameResult;
pub use symbol_hierarchy::SymbolTree;
pub use syntax_highlighting::{HlAttrField, HlKeyword, HlOperator, HlPunct, HlRange, HlTag};

pub const DEFAULT_LRU_CAP: usize = 128;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NavigationTarget {
    pub file_id: FileId,
    pub full_range: TextRange,
    pub focus_range: TextRange,
}

pub use salsa::Cancelled;

pub type Cancellable<T> = Result<T, Cancelled>;

#[salsa::database(SourceDatabaseStorage, DefDatabaseStorage, TyDatabaseStorage)]
struct RootDatabase {
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

impl Default for RootDatabase {
    fn default() -> Self {
        use crate::SourceDatabase;

        let mut db = Self {
            storage: salsa::Storage::default(),
        };

        crate::def::ParseQuery
            .in_db_mut(&mut db)
            .set_lru_capacity(DEFAULT_LRU_CAP);

        db.set_flake_graph_with_durability(Arc::default(), Durability::MEDIUM);
        db.set_nixos_options_with_durability(Arc::default(), Durability::MEDIUM);
        db
    }
}

#[derive(Default, Debug)]
pub struct AnalysisHost {
    db: RootDatabase,
}

impl AnalysisHost {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_single_file(src: &str) -> (Self, FileId) {
        let mut change = Change::default();
        let file = FileId(0);
        change.change_file(file, src.into());
        let mut file_set = FileSet::default();
        file_set.insert(file, VfsPath::new(format!("/{DEFAULT_IMPORT_FILE}")));
        change.set_roots(vec![SourceRoot::new_local(file_set, Some(file))]);
        let mut this = Self::new();
        this.apply_change(change);
        (this, file)
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

    //// LSP standard ////

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

    pub fn completions(
        &self,
        pos: FilePos,
        trigger_char: Option<char>,
    ) -> Cancellable<Option<Vec<CompletionItem>>> {
        self.with_db(|db| completion::completions(db, pos, trigger_char))
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

    pub fn links(&self, file: FileId) -> Cancellable<Vec<Link>> {
        self.with_db(|db| links::links(db, file))
    }

    pub fn link_resolve(&self, frange: FileRange) -> Cancellable<Option<Link>> {
        self.with_db(|db| links::link_resolve(db, frange))
    }

    pub fn assists(&self, frange: FileRange) -> Cancellable<Vec<Assist>> {
        self.with_db(|db| assists::assists(db, frange))
    }

    pub fn highlight_related(&self, fpos: FilePos) -> Cancellable<Vec<HlRelated>> {
        self.with_db(|db| highlight_related::highlight_related(db, fpos).unwrap_or_default())
    }

    //// Custom extensions ////

    pub fn file_references(&self, file: FileId) -> Cancellable<Vec<FileId>> {
        self.with_db(|db| file_references::file_references(db, file))
    }

    pub fn file_referrers(&self, file: FileId) -> Cancellable<Vec<FileId>> {
        self.with_db(|db| file_references::file_referrers(db, file))
    }
}
