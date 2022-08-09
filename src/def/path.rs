use super::DefDatabase;
use crate::FileId;
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
    pub fn data(self, db: &dyn DefDatabase) -> PathData {
        db.lookup_intern_path(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathData {
    anchor: PathAnchor,
    supers: u8,
    // Normalized path separated by `/`, with no `.` or `..` segments.
    raw_segments: SmolStr,
}

impl PathData {
    pub(crate) fn normalize(anchor: PathAnchor, segments: &str) -> Self {
        let mut raw_segments = String::new();
        let mut supers = 0u8;
        for seg in segments
            .split('/')
            .filter(|&seg| !seg.is_empty() && seg != ".")
        {
            if seg != ".." {
                if !raw_segments.is_empty() {
                    raw_segments.push('/');
                }
                raw_segments.push_str(seg);
            } else if raw_segments.is_empty() {
                // Extra ".." has no effect for absolute path.
                if anchor != PathAnchor::Absolute {
                    supers = supers.saturating_add(1);
                }
            } else {
                let last_slash = raw_segments.bytes().rposition(|c| c == b'/').unwrap_or(0);
                raw_segments.truncate(last_slash);
            }
        }
        Self {
            anchor,
            supers,
            raw_segments: raw_segments.into(),
        }
    }

    pub fn anchor(&self) -> &PathAnchor {
        &self.anchor
    }

    pub fn supers(&self) -> u8 {
        self.supers
    }

    pub fn segments(&self) -> impl Iterator<Item = &str> + '_ {
        self.raw_segments.split(' ').filter(|s| !s.is_empty())
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
    #[rustfmt::skip]
    fn normalize_relative() {
        for anchor in [PathAnchor::Relative(FileId(0)), PathAnchor::Home, PathAnchor::Search("foo".into())] {
            let norm = |s| PathData::normalize(anchor.clone(), s);
            assert_eq!(norm(""), PathData { anchor: anchor.clone(), supers: 0, raw_segments: "".into() });
            assert_eq!(norm("./."), PathData { anchor: anchor.clone(), supers: 0, raw_segments: "".into() });
            assert_eq!(norm("./.."), PathData { anchor: anchor.clone(), supers: 1, raw_segments: "".into() });
            assert_eq!(norm("../."), PathData { anchor: anchor.clone(), supers: 1, raw_segments: "".into() });
            assert_eq!(norm("foo/./bar/../.baz"), PathData { anchor: anchor.clone(), supers: 0, raw_segments: "foo/.baz".into() });
            assert_eq!(norm("../../foo"), PathData { anchor: anchor.clone(), supers: 2, raw_segments: "foo".into() });
        }
    }

    #[test]
    #[rustfmt::skip]
    fn normalize_absolute() {
        let anchor = PathAnchor::Absolute;
        let norm = |s| PathData::normalize(anchor.clone(), s);
        assert_eq!(norm("/./."), PathData { anchor: anchor.clone(), supers: 0, raw_segments: "".into() });
        assert_eq!(norm("/./.."), PathData { anchor: anchor.clone(), supers: 0, raw_segments: "".into() });
        assert_eq!(norm("/../."), PathData { anchor: anchor.clone(), supers: 0, raw_segments: "".into() });
        assert_eq!(norm("/foo/./bar/../.baz"), PathData { anchor: anchor.clone(), supers: 0, raw_segments: "foo/.baz".into() });
        assert_eq!(norm("/../../foo"), PathData { anchor: anchor.clone(), supers: 0, raw_segments: "foo".into() });
    }
}
