use indexmap::IndexMap;
use lsp_types::Url;
use nil::{Change, FileId, FilePos};
use std::{fmt, mem, path::PathBuf, sync::Arc};
use text_size::TextSize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VfsPath(PathBuf);

impl<'a> TryFrom<&'a Url> for VfsPath {
    type Error = ();
    fn try_from(url: &'a Url) -> Result<Self, Self::Error> {
        url.to_file_path().map(Self)
    }
}

impl<'a> From<&'a VfsPath> for Url {
    fn from(path: &'a VfsPath) -> Self {
        Url::from_file_path(&path.0).unwrap()
    }
}

#[derive(Default)]
pub struct Vfs {
    files: IndexMap<VfsPath, Option<(Arc<str>, LineMap)>>,
    change: Change,
}

impl fmt::Debug for Vfs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Files(Vec<(usize, VfsPath)>);
        impl fmt::Debug for Files {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_map()
                    .entries(self.0.iter().map(|(k, v)| (k, v)))
                    .finish()
            }
        }

        let files = Files(self.files.keys().cloned().enumerate().collect());
        f.debug_struct("Vfs")
            .field("files", &files)
            .finish_non_exhaustive()
    }
}

impl Vfs {
    pub fn file_id(&self, path: &VfsPath) -> Option<FileId> {
        self.files.get_index_of(path).map(|id| FileId(id as _))
    }

    pub fn file_path(&self, file_id: FileId) -> Option<&VfsPath> {
        self.files.get_index(file_id.0 as _).map(|(path, _)| path)
    }

    pub fn take_change(&mut self) -> Change {
        mem::take(&mut self.change)
    }

    pub fn set_file_content(&mut self, path: VfsPath, content: Option<String>) -> FileId {
        let text_with_map = content
            .and_then(LineMap::normalize)
            .map(|(text, map)| (text.into(), map));
        let text = text_with_map.as_ref().map(|(text, _)| Arc::clone(text));
        let id = self.files.insert_full(path, text_with_map).0;
        let file_id = FileId(u32::try_from(id).unwrap());
        self.change.change_file(file_id, text);
        file_id
    }

    pub fn get_file_pos(&self, path: &VfsPath, line: u32, col: u32) -> Option<FilePos> {
        let (id, _, inner) = self.files.get_full(path)?;
        let (_, line_map) = inner.as_ref()?;
        let pos = line_map.pos(line, col);
        Some(FilePos::new(FileId(id as _), pos))
    }

    pub fn get_file_line_col(&self, file_pos: FilePos) -> Option<(&VfsPath, u32, u32)> {
        let (path, inner) = self.files.get_index(file_pos.file_id.0 as usize)?;
        let (_, line_map) = inner.as_ref()?;
        let (line, col) = line_map.line_col(file_pos.value);
        Some((path, line, col))
    }
}

#[derive(Default, Debug)]
struct LineMap {
    line_starts: Vec<u32>,
}

impl LineMap {
    fn normalize(text: String) -> Option<(String, Self)> {
        // Too large for `TextSize`.
        if text.len() > u32::MAX as usize {
            return None;
        }
        // TODO: UTF-16 handling of non-ASCII chars and "\r\n" handling.
        if text.bytes().any(|b| !b.is_ascii() || b == b'\r') {
            return None;
        }
        let line_starts = Some(0)
            .into_iter()
            .chain(
                text.bytes()
                    .zip(0u32..)
                    .filter(|(b, _)| *b == b'\n')
                    .map(|(_, i)| i + 1),
            )
            .collect();
        Some((text, Self { line_starts }))
    }

    fn pos(&self, line: u32, col: u32) -> TextSize {
        (self.line_starts.get(line as usize).copied().unwrap_or(0) + col).into()
    }

    fn line_col(&self, pos: TextSize) -> (u32, u32) {
        let pos = u32::from(pos);
        let line = self
            .line_starts
            .partition_point(|&i| i <= pos)
            .saturating_sub(1);
        let col = pos - self.line_starts[line];
        (line as u32, col as u32)
    }
}

#[cfg(test)]
mod tests {
    use super::LineMap;

    #[test]
    fn line_map() {
        let (s, map) = LineMap::normalize("hello\nworld\nend".into()).unwrap();
        assert_eq!(s, "hello\nworld\nend");
        assert_eq!(&map.line_starts, &[0, 6, 12]);

        let mapping = [
            (0, 0, 0),
            (2, 0, 2),
            (5, 0, 5),
            (6, 1, 0),
            (11, 1, 5),
            (12, 2, 0),
        ];
        for (pos, line, col) in mapping {
            assert_eq!(map.line_col(pos.into()), (line, col));
            assert_eq!(map.pos(line, col), pos.into());
        }
    }
}
