use crate::base::{SourceDatabase, SourceDatabaseStorage};
use crate::def::DefDatabaseStorage;
use crate::{Change, FileId};
use rowan::{ast::AstNode, TextSize};
use syntax::NixLanguage;

pub const CURSOR_MARKER: &str = "$0";

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
        let mut change = Change::new();
        change.change_file(root_id, Some(content.into()));
        change.apply(&mut db);
        (db, root_id)
    }

    pub fn from_file_with_pos(content: &str) -> (Self, FileId, TextSize) {
        let (pos, content) = extract_pos(content).expect("Missing cursor marker");
        let (this, root_id) = Self::from_file(&content);
        (this, root_id, pos)
    }

    pub fn node_at<N: AstNode<Language = NixLanguage>>(&self, file_id: FileId, pos: TextSize) -> N {
        self.parse(file_id)
            .value
            .syntax_node()
            .token_at_offset(pos)
            .right_biased()
            .expect("No token")
            .parent_ancestors()
            .find_map(N::cast)
            .expect("No node found")
    }
}

fn extract_pos(content: &str) -> Option<(TextSize, String)> {
    let pos = content.find(CURSOR_MARKER)?;
    let rest = content[..pos].to_owned() + &content[pos + CURSOR_MARKER.len()..];
    let pos = TextSize::try_from(pos).ok()?;
    Some((pos, rest))
}
