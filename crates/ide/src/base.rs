use nix_interop::flake_output::FlakeOutput;
use nix_interop::nixos_options::NixosOptions;
use salsa::Durability;
use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use syntax::{TextRange, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceRootId(pub u32);

/// An path in the virtual filesystem.
#[cfg(unix)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VfsPath {
    Path(PathBuf),
    Virtual(String),
}

impl VfsPath {
    /// Construct a new filesystem path.
    #[must_use]
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self::Path(path.as_ref().to_path_buf())
    }

    /// Return a reference to the underlying `Path`.
    #[must_use]
    pub fn as_path(&self) -> Option<&Path> {
        match self {
            Self::Path(path) => Some(path),
            Self::Virtual(_) => None,
        }
    }

    /// Extends `self` with `path`.
    ///
    /// Returns `None` if the path is not extentable, otherwise returns `Some(())`.
    #[must_use]
    pub fn push(&mut self, path: &str) -> Option<()> {
        match self {
            Self::Path(this) => {
                this.push(path);
                Some(())
            }
            Self::Virtual(_) => None,
        }
    }

    /// Creates a new `VfsPath` with `path` adjoined to self.
    #[must_use]
    pub fn join(&self, path: &str) -> Option<Self> {
        match self {
            Self::Path(this) => Some(Self::Path(this.join(path))),
            Self::Virtual(_) => None,
        }
    }

    /// Truncates `self` to the parent of it.
    ///
    /// Returns `false` and does nothing if `self` has no parent,
    /// otherwise, return `true`.
    pub fn pop(&mut self) -> bool {
        match self {
            Self::Path(this) => this.pop(),
            Self::Virtual(_) => false,
        }
    }

    /// Returns an human readable `impl Display` struct.
    #[must_use]
    pub fn display(&self) -> impl fmt::Display + '_ {
        struct Display<'a>(&'a VfsPath);

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.0 {
                    VfsPath::Path(path) => path.display().fmt(f),
                    VfsPath::Virtual(id) => write!(f, "(virtual path {id})"),
                }
            }
        }

        Display(self)
    }
}

impl From<PathBuf> for VfsPath {
    fn from(path: PathBuf) -> Self {
        Self::Path(path)
    }
}

impl From<&'_ Path> for VfsPath {
    fn from(path: &'_ Path) -> Self {
        Self::Path(path.to_path_buf())
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

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.files.get(path).copied()
    }

    pub fn path_for_file(&self, file: FileId) -> &VfsPath {
        &self.paths[&file]
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (FileId, &'_ VfsPath)> + '_ {
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
    /// A list of files.
    file_set: FileSet,
    /// The entry file.
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

    pub fn files(&self) -> impl ExactSizeIterator<Item = (FileId, &'_ VfsPath)> + '_ {
        self.file_set.iter()
    }

    pub fn entry(&self) -> Option<FileId> {
        self.entry
    }
}

/// A mapping from [SourceRootId] to [FlakeInfo]
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct FlakeGraph {
    pub nodes: HashMap<SourceRootId, FlakeInfo>,
}

// FIXME: Make this a tree structure.
#[derive(Clone, PartialEq, Eq)]
pub struct FlakeInfo {
    /// Id related with the flake content.
    pub flake_file: FileId,
    /// The flake inputs.
    pub input_store_paths: HashMap<String, VfsPath>,
    /// The flake outputs.
    pub input_flake_outputs: HashMap<String, FlakeOutput>,
}

impl fmt::Debug for FlakeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FlakeInfo")
            .field("flake_file", &self.flake_file)
            .field("input_store_paths", &self.input_store_paths)
            .field("input_flake_outputs", &self.input_flake_outputs.keys())
            .finish_non_exhaustive()
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

/// A position inside of a file.
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

/// A range inside of a file.
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
}

/// The DB storing files.
/// [FileID] and [SourceRootId] are used.
#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_content(&self, file_id: FileId) -> Arc<str>;

    #[salsa::input]
    fn source_root(&self, sid: SourceRootId) -> Arc<SourceRoot>;

    fn source_root_flake_info(&self, sid: SourceRootId) -> Option<Arc<FlakeInfo>>;

    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;

    #[salsa::input]
    fn flake_graph(&self) -> Arc<FlakeGraph>;

    #[salsa::input]
    fn nixos_options(&self) -> Arc<NixosOptions>;
}

/// Return the flake info for a [SourceRootId].
fn source_root_flake_info(db: &dyn SourceDatabase, sid: SourceRootId) -> Option<Arc<FlakeInfo>> {
    db.flake_graph().nodes.get(&sid).cloned().map(Arc::new)
}

/// A change to the [SourceDataBase].
/// Use [Self::apply] to apply all changes to the DB.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct Change {
    pub flake_graph: Option<FlakeGraph>,
    pub roots: Option<Vec<SourceRoot>>,
    pub file_changes: Vec<(FileId, Arc<str>)>,
    pub nixos_options: Option<NixosOptions>,
}

impl Change {
    pub fn is_empty(&self) -> bool {
        self.roots.is_none() && self.file_changes.is_empty()
    }

    pub fn set_flake_graph(&mut self, graph: FlakeGraph) {
        self.flake_graph = Some(graph);
    }

    pub fn set_nixos_options(&mut self, opts: NixosOptions) {
        self.nixos_options = Some(opts);
    }

    pub fn set_roots(&mut self, roots: Vec<SourceRoot>) {
        self.roots = Some(roots);
    }

    pub fn change_file(&mut self, file_id: FileId, content: Arc<str>) {
        self.file_changes.push((file_id, content));
    }

    pub(crate) fn apply(self, db: &mut dyn SourceDatabase) {
        if let Some(flake_graph) = self.flake_graph {
            db.set_flake_graph_with_durability(Arc::new(flake_graph), Durability::MEDIUM);
        }
        if let Some(opts) = self.nixos_options {
            db.set_nixos_options_with_durability(Arc::new(opts), Durability::MEDIUM);
        }
        if let Some(roots) = self.roots {
            u32::try_from(roots.len()).expect("Length overflow");
            for (sid, root) in (0u32..).map(SourceRootId).zip(roots) {
                for (fid, _) in root.files() {
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
