use crate::source::{FileId, SourceDatabase};
use crate::RootDatabase;

pub trait FixtureExt: Default + SourceDatabase {
    fn from_single_file(content: &str) -> (Self, FileId) {
        let mut db = Self::default();
        let root_id = FileId(0);
        db.set_file_content(root_id, content.as_bytes().into());
        db.set_file_source_root(root_id);
        (db, root_id)
    }
}

impl FixtureExt for RootDatabase {}
