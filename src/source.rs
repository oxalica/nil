use rnix::types as ast;
use std::sync::Arc;

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
}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_source_root(&self) -> FileId;

    #[salsa::input]
    fn file_content(&self, file_id: FileId) -> Arc<[u8]>;

    fn parse(&self, file_id: FileId) -> InFile<ast::Root>;
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> InFile<ast::Root> {
    let content = db.file_content(file_id);
    let content = std::str::from_utf8(&content).unwrap_or_default();
    let ast = rnix::parse(content);
    InFile::new(file_id, ast.root())
}
