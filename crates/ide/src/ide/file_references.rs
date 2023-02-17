use crate::{DefDatabase, FileId};

pub(crate) fn file_references(db: &dyn DefDatabase, file: FileId) -> Vec<FileId> {
    let mut refs = db
        .module_references(file)
        .iter()
        .copied()
        .collect::<Vec<_>>();
    refs.sort();
    refs
}

pub(crate) fn file_referrers(db: &dyn DefDatabase, file: FileId) -> Vec<FileId> {
    db.module_referrers(file).into_vec()
}
