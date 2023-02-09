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
    pub(crate) fn resolve_path_query(db: &dyn DefDatabase, path: Path) -> Option<VfsPath> {
        let data = path.data(db);
        let file = match &data.anchor {
            &PathAnchor::Relative(file) => file,
            // TODO
            PathAnchor::Absolute | PathAnchor::Home | PathAnchor::Search(_) => return None,
        };

        let sid = db.file_source_root(file);
        let root = db.source_root(sid);
        let mut vpath = root.path_for_file(file).clone();

        // Virtual paths are all standalone.
        if matches!(vpath, VfsPath::Virtual(_)) {
            return None;
        }

        for _ in 0..(data.supers.saturating_add(1)) {
            // Allows extra `..`s.
            vpath.pop();
        }
        vpath.push(&data.relative_path)?;
        Some(vpath)
    }

    pub fn data(self, db: &dyn DefDatabase) -> PathData {
        db.lookup_intern_path(self)
    }

    pub fn resolve(self, db: &dyn DefDatabase) -> Option<VfsPath> {
        db.resolve_path(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathData {
    anchor: PathAnchor,
    supers: u8,
    // Normalized relative path separated by `/`.
    relative_path: Box<str>,
}

impl PathData {
    pub(crate) fn normalize(anchor: PathAnchor, segments: &str) -> Self {
        let mut relative_path = String::with_capacity(segments.len());
        let mut supers = 0u8;
        for seg in segments
            .split('/')
            .filter(|&seg| !seg.is_empty() && seg != ".")
        {
            if seg != ".." {
                relative_path.push_str(seg);
                relative_path.push('/');
            } else if !relative_path.is_empty() {
                relative_path.pop();
                relative_path.truncate(relative_path.rfind('/').map_or(0, |i| i + 1));
                // Extra ".." has no effect for absolute path.
            } else if anchor != PathAnchor::Absolute {
                supers = supers.saturating_add(1);
            }
        }

        // Trailing `/` if there are any segments.
        relative_path.pop();

        Self {
            anchor,
            supers,
            relative_path: relative_path.into_boxed_str(),
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
    use crate::FileId;

    #[test]
    fn normalize_relative() {
        for anchor in [
            PathAnchor::Relative(FileId(0)),
            PathAnchor::Home,
            PathAnchor::Search("foo".into()),
        ] {
            let norm = |s| PathData::normalize(anchor.clone(), s);
            let path = |supers, p: &str| PathData {
                anchor: anchor.clone(),
                supers,
                relative_path: p.into(),
            };
            assert_eq!(norm(""), path(0, ""));
            assert_eq!(norm("./."), path(0, ""));
            assert_eq!(norm("./.."), path(1, ""));
            assert_eq!(norm("../."), path(1, ""));
            assert_eq!(norm("foo/./bar/../.baz"), path(0, "foo/.baz"));
            assert_eq!(norm("../../foo"), path(2, "foo"));
        }
    }

    #[test]
    fn normalize_absolute() {
        let anchor = PathAnchor::Absolute;
        let norm = |s| PathData::normalize(anchor.clone(), s);
        let path = |p: &str| PathData {
            anchor: anchor.clone(),
            supers: 0,
            relative_path: p.into(),
        };
        assert_eq!(norm("/"), path(""));
        assert_eq!(norm("./."), path(""));
        assert_eq!(norm("./.."), path(""));
        assert_eq!(norm("../."), path(""));
        assert_eq!(norm("foo/./bar/../.baz"), path("foo/.baz"));
        assert_eq!(norm("../../foo"), path("foo"));
    }
}
