use rowan::{TextRange, TextSize};
use salsa::Durability;
use std::fmt;
use std::sync::Arc;
use syntax::Parse;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

impl<T> InFile<T> {
    pub fn new(file_id: FileId, value: T) -> Self {
        Self { file_id, value }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> InFile<U> {
        InFile {
            file_id: self.file_id,
            value: f(self.value),
        }
    }
}

pub type FilePos = InFile<TextSize>;
pub type FileRange = InFile<TextRange>;

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_content(&self, file_id: FileId) -> Arc<str>;

    fn parse(&self, file_id: FileId) -> Parse;
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse {
    let content = db.file_content(file_id);
    syntax::parse_file(&content)
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Change {
    pub file_changes: Vec<(FileId, Option<Arc<str>>)>,
}

impl Change {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.file_changes.is_empty()
    }

    pub fn change_file(&mut self, file_id: FileId, content: Option<Arc<str>>) {
        self.file_changes.push((file_id, content));
    }

    pub(crate) fn apply(self, db: &mut dyn SourceDatabase) {
        for (file_id, content) in self.file_changes {
            let content = content.unwrap_or_else(|| String::new().into());
            // TODO: Better guess of durability?
            db.set_file_content_with_durability(file_id, content, Durability::HIGH);
        }
    }
}

impl fmt::Debug for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let modified = self
            .file_changes
            .iter()
            .filter(|(_, content)| content.is_some())
            .count();
        let cleared = self.file_changes.len() - modified;
        f.debug_struct("Change")
            .field("modified", &modified)
            .field("cleared", &cleared)
            .finish_non_exhaustive()
    }
}
