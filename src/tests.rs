use crate::def::DefDatabaseStorage;
use crate::source::{FileId, SourceDatabase, SourceDatabaseStorage};

#[salsa::database(SourceDatabaseStorage, DefDatabaseStorage)]
#[derive(Default)]
pub struct TestDB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDB {}

impl TestDB {
    pub fn from_file(content: &str) -> (Self, FileId) {
        let mut db = Self::default();
        let root_id = FileId(0);
        db.set_file_content(root_id, content.as_bytes().into());
        db.set_file_source_root(root_id);
        (db, root_id)
    }
}
