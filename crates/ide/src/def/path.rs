use super::DefDatabase;
use crate::{FileId, VfsPath};
use smol_str::SmolStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path(salsa::InternId);

impl salsa::InternKey for Path {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

impl Path {
    pub(crate) fn resolve_path_query(db: &dyn DefDatabase, path: Path) -> Option<FileId> {
        let data = path.data(db);
        let file = match &data.anchor {
            &PathAnchor::Relative(file) => file,
            // TODO
            PathAnchor::Absolute | PathAnchor::Home | PathAnchor::Search(_) => return None,
        };
        let sid = db.file_source_root(file);
        let root = db.source_root(sid);
        let mut vpath = root.path_for_file(file).clone();
        for _ in 0..(data.supers.saturating_add(1)) {
            vpath.pop()?;
        }
        vpath.push(&data.relative);
        root.get_file_for_path(&vpath).or_else(|| {
            vpath.push_segment("default.nix").unwrap();
            root.get_file_for_path(&vpath)
        })
    }

    pub fn data(self, db: &dyn DefDatabase) -> PathData {
        db.lookup_intern_path(self)
    }

    pub fn resolve(self, db: &dyn DefDatabase) -> Option<FileId> {
        db.resolve_path(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathData {
    anchor: PathAnchor,
    supers: u8,
    // Normalized relative path.
    relative: VfsPath,
}

impl PathData {
    pub(crate) fn normalize(anchor: PathAnchor, segments: &str) -> Self {
        let mut relative = VfsPath::root();
        let mut supers = 0u8;
        for seg in segments
            .split('/')
            .filter(|&seg| !seg.is_empty() && seg != ".")
        {
            if seg != ".." {
                relative.push_segment(seg).expect("Checked by lexer");
            // Extra ".." has no effect for absolute path.
            } else if relative.pop().is_none() && anchor != PathAnchor::Absolute {
                supers = supers.saturating_add(1);
            }
        }
        Self {
            anchor,
            supers,
            relative,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathAnchor {
    Relative(FileId),
    Absolute,
    Home,
    Search(SmolStr),
}

#[cfg(test)]
mod tests {
    use super::{PathAnchor, PathData};
    use crate::{FileId, VfsPath};

    #[test]
    #[rustfmt::skip]
    fn normalize_relative() {
        for anchor in [PathAnchor::Relative(FileId(0)), PathAnchor::Home, PathAnchor::Search("foo".into())] {
            let norm = |s| PathData::normalize(anchor.clone(), s);
            let path = |supers, p: &str| PathData { anchor: anchor.clone(), supers, relative: VfsPath::new(p).unwrap() };
            assert_eq!(norm(""), path(0, ""));
            assert_eq!(norm("./."), path(0, ""));
            assert_eq!(norm("./.."), path(1, ""));
            assert_eq!(norm("../."), path(1, ""));
            assert_eq!(norm("foo/./bar/../.baz"), path(0, "foo/.baz"));
            assert_eq!(norm("../../foo"), path(2, "foo"));
        }
    }

    #[test]
    #[rustfmt::skip]
    fn normalize_absolute() {
        let anchor = PathAnchor::Absolute;
        let norm = |s| PathData::normalize(anchor.clone(), s);
            let path = |p: &str| PathData { anchor: anchor.clone(), supers: 0, relative: VfsPath::new(p).unwrap() };
        assert_eq!(norm("/"), path(""));
        assert_eq!(norm("./."), path(""));
        assert_eq!(norm("./.."), path(""));
        assert_eq!(norm("../."), path(""));
        assert_eq!(norm("foo/./bar/../.baz"), path("foo/.baz"));
        assert_eq!(norm("../../foo"), path("foo"));
    }
}
