use salsa::Durability;
use std::collections::HashMap;
use std::fmt;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;
use syntax::{TextRange, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceRootId(pub u32);

/// An absolute Unix-like path in the virtual filesystem.
///
/// It must be in form `(/[^/]+)+` and every segment must be non-empty and not `.` or `..`.
/// The root represented by an empty path.
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct VfsPath(String);

impl VfsPath {
    pub fn root() -> Self {
        Self::default()
    }

    /// Construct a new absolute path in Unix-like format.
    pub fn new(s: impl Into<String>) -> Result<Self, ParseVfsPathError> {
        let s = s.into();
        if s == "/" {
            return Ok(Self::root());
        }
        if !s.starts_with('/') || s[1..].split('/').any(|seg| matches!(seg, "" | "." | "..")) {
            return Err(ParseVfsPathError);
        }
        Ok(Self(s))
    }

    /// Append a path segment at the end and return the new path.
    /// Panic if it is empty or contains `/`.
    pub fn join_segment(&self, segment: &str) -> Self {
        assert!(!segment.is_empty() && !segment.contains('/'));
        let mut buf = String::with_capacity(self.0.len() + 1 + segment.len());
        buf += &self.0;
        buf += "/";
        buf += segment;
        Self(buf)
    }

    /// Assume another VfsPath as relative and append it to this one.
    pub fn append(&mut self, relative: &Self) {
        self.0.push_str(&relative.0);
    }

    /// Push a path segment at the end.
    /// Panic if it is empty or contains `/`.
    pub fn push_segment(&mut self, segment: &str) {
        assert!(!segment.is_empty() && !segment.contains('/'));
        self.0 += "/";
        self.0 += segment;
    }

    /// Pop the last segment from the end.
    /// Returns `None` if it is already the root.
    pub fn pop(&mut self) -> Option<()> {
        self.0.truncate(self.0.rsplit_once('/')?.0.len());
        Some(())
    }

    /// Get the path in Unix-like form.
    pub fn as_str(&self) -> &str {
        if !self.0.is_empty() {
            &self.0
        } else {
            "/"
        }
    }
}

impl TryFrom<PathBuf> for VfsPath {
    type Error = ParseVfsPathError;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        (&*path).try_into()
    }
}

impl TryFrom<&'_ Path> for VfsPath {
    type Error = ParseVfsPathError;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        let mut ret = Self(String::with_capacity(path.as_os_str().len()));
        for comp in path.components() {
            match comp {
                Component::RootDir => {}
                Component::Normal(seg) => {
                    ret.push_segment(seg.to_str().ok_or(ParseVfsPathError)?);
                }
                _ => return Err(ParseVfsPathError),
            }
        }
        Ok(ret)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct ParseVfsPathError;

impl fmt::Display for ParseVfsPathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Invalid VfsPath")
    }
}

impl std::error::Error for ParseVfsPathError {}

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

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.files.get(path).copied()
    }

    pub fn path_for_file(&self, file: FileId) -> &VfsPath {
        &self.paths[&file]
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
    entry: Option<FileId>,
}

impl SourceRoot {
    pub fn new_local(file_set: FileSet, entry: Option<FileId>) -> Self {
        Self { file_set, entry }
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.file_set.file_for_path(path)
    }

    pub fn path_for_file(&self, file: FileId) -> &VfsPath {
        self.file_set.path_for_file(file)
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &'_ VfsPath)> + '_ {
        self.file_set.iter()
    }

    pub fn entry(&self) -> Option<FileId> {
        self.entry
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FilePos {
    pub file_id: FileId,
    pub pos: TextSize,
}

impl FilePos {
    pub fn new(file_id: FileId, pos: TextSize) -> Self {
        Self { file_id, pos }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FileRange {
    pub file_id: FileId,
    pub range: TextRange,
}

impl FileRange {
    pub fn new(file_id: FileId, range: TextRange) -> Self {
        Self { file_id, range }
    }

    pub fn empty(pos: FilePos) -> Self {
        Self::new(pos.file_id, TextRange::empty(pos.pos))
    }

    pub fn span(start: FilePos, end: FilePos) -> Self {
        assert_eq!(start.file_id, end.file_id);
        Self::new(start.file_id, TextRange::new(start.pos, end.pos))
    }
}

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
    pub file_changes: Vec<(FileId, Arc<str>)>,
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

    pub fn change_file(&mut self, file_id: FileId, content: Arc<str>) {
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
            db.set_file_content_with_durability(file_id, content, Durability::LOW);
        }
    }
}

impl fmt::Debug for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let modified = self
            .file_changes
            .iter()
            .filter(|(_, content)| !content.is_empty())
            .count();
        let cleared = self.file_changes.len() - modified;
        f.debug_struct("Change")
            .field("roots", &self.roots.as_ref().map(|roots| roots.len()))
            .field("modified", &modified)
            .field("cleared", &cleared)
            .finish_non_exhaustive()
    }
}
