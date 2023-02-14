use crate::FileId;
use smol_str::SmolStr;
use std::collections::HashMap;
use syntax::TextRange;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceEdit {
    pub content_edits: HashMap<FileId, Vec<TextEdit>>,
    // Filesystem edit is not implemented yet.
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextEdit {
    pub delete: TextRange,
    pub insert: SmolStr,
}

impl TextEdit {
    pub fn apply(&self, src: &mut String) {
        let delete_range = usize::from(self.delete.start())..usize::from(self.delete.end());
        src.replace_range(delete_range, &self.insert);
    }
}
