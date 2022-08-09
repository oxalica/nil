use rowan::{TextRange, TextSize};
use salsa::Durability;
use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceRootId(pub u32);

/// An absolute path in format `(/.+)*`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VfsPath(String);

impl VfsPath {
    pub fn from_path(p: &Path) -> Option<Self> {
        Self::new(p.to_str()?)
    }

    pub fn new(s: impl Into<String>) -> Option<Self> {
        let mut s: String = s.into();
        if s.is_empty() || s == "/" {
            return Some(Self(String::new()));
        }
        if s.ends_with('/') || s.as_bytes().windows(2).any(|w| w == b"//") {
            return None;
        }
        if !s.starts_with('/') {
            s.insert(0, '/');
        }
        Some(Self(s))
    }

    pub fn push(&mut self, relative: &Self) {
        self.0.push_str(&relative.0);
    }

    pub fn pop(&mut self) -> Option<()> {
        self.0.truncate(self.0.rsplit_once('/')?.0.len());
        Some(())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A set of [`VfsPath`]s identified by [`FileId`]s.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct FileSet {
    files: HashMap<VfsPath, FileId>,
    paths: HashMap<FileId, VfsPath>,
}

impl FileSet {
    pub fn insert(&mut self, file: FileId, path: VfsPath) {
        self.files.insert(path.clone(), file);
        self.paths.insert(file, path);
    }

    pub fn remove_file(&mut self, file: FileId) {
        if let Some(path) = self.paths.remove(&file) {
            self.files.remove(&path);
        }
    }

    pub fn get_file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.files.get(path).copied()
    }

    pub fn get_path_for_file(&self, file: FileId) -> Option<&VfsPath> {
        self.paths.get(&file)
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &'_ VfsPath)> + '_ {
        self.paths.iter().map(|(&file, path)| (file, path))
    }
}

impl fmt::Debug for FileSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(&self.paths).finish()
    }
}

/// A workspace unit, typically a Flake package.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceRoot {
    file_set: FileSet,
}

impl SourceRoot {
    pub fn new_local(file_set: FileSet) -> Self {
        Self { file_set }
    }

    pub fn get_file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.file_set.get_file_for_path(path)
    }

    pub fn get_path_for_file(&self, file: FileId) -> Option<&VfsPath> {
        self.file_set.get_path_for_file(file)
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &'_ VfsPath)> + '_ {
        self.file_set.iter()
    }
}

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

    #[salsa::input]
    fn source_root(&self, id: SourceRootId) -> Arc<SourceRoot>;

    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Change {
    pub roots: Option<Vec<SourceRoot>>,
    pub file_changes: Vec<(FileId, Option<Arc<str>>)>,
}

impl Change {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.roots.is_none() && self.file_changes.is_empty()
    }

    pub fn set_roots(&mut self, roots: Vec<SourceRoot>) {
        self.roots = Some(roots);
    }

    pub fn change_file(&mut self, file_id: FileId, content: Option<Arc<str>>) {
        self.file_changes.push((file_id, content));
    }

    pub(crate) fn apply(self, db: &mut dyn SourceDatabase) {
        if let Some(roots) = self.roots {
            u32::try_from(roots.len()).expect("Length overflow");
            for (sid, root) in (0u32..).map(SourceRootId).zip(roots) {
                for (fid, _) in root.iter() {
                    db.set_file_source_root_with_durability(fid, sid, Durability::HIGH);
                }
                db.set_source_root_with_durability(sid, Arc::new(root), Durability::HIGH);
            }
        }
        for (file_id, content) in self.file_changes {
            let content = content.unwrap_or_else(|| String::new().into());
            db.set_file_content_with_durability(file_id, content, Durability::LOW);
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
            .field("roots", &self.roots.as_ref().map(|roots| roots.len()))
            .field("modified", &modified)
            .field("cleared", &cleared)
            .finish_non_exhaustive()
    }
}
