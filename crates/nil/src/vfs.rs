use crate::UrlExt;
use anyhow::{ensure, Context, Result};
use ide::{Change, FileId, FileSet, FlakeGraph, FlakeInfo, SourceRoot, SourceRootId, VfsPath};
use lsp_types::Url;
use std::collections::HashMap;
use std::sync::Arc;
use std::{fmt, mem};
use text_size::{TextRange, TextSize};

/// Vfs stores file contents with line mapping, and a mapping between
/// filesystem paths and `FileId`s.
/// The query system is built on `FileId`'s.
pub struct Vfs {
    // FIXME: Currently this list is append-only.
    files: Vec<(Arc<str>, Arc<LineMap>)>,
    local_file_set: FileSet,
    root_changed: bool,
    change: Change,
}

impl fmt::Debug for Vfs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Vfs")
            .field("file_cnt", &self.files.len())
            .field("root_changed", &self.root_changed)
            .field("change", &self.change)
            .finish_non_exhaustive()
    }
}

impl Vfs {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            local_file_set: FileSet::default(),
            root_changed: false,
            change: Change::default(),
        }
    }

    pub fn set_flake_info(&mut self, flake_info: Option<FlakeInfo>) {
        self.change.set_flake_graph(FlakeGraph {
            nodes: HashMap::from_iter(flake_info.map(|info| (SourceRootId(0), info))),
        });
    }

    pub fn set_path_content(&mut self, path: VfsPath, text: String) -> FileId {
        let (text, line_map) = LineMap::normalize(text);
        let text = <Arc<str>>::from(text);
        let line_map = Arc::new(line_map);
        match self.local_file_set.file_for_path(&path) {
            Some(file) => {
                self.files[file.0 as usize] = (text.clone(), line_map);
                self.change.change_file(file, text);
                self.local_file_set.remove_file(file);
                self.root_changed = true;
                file
            }
            None => {
                let file = FileId(u32::try_from(self.files.len()).expect("Length overflow"));
                self.local_file_set.insert(file, path);
                self.root_changed = true;
                self.files.push((text.clone(), line_map));
                self.change.change_file(file, text);
                file
            }
        }
    }

    pub fn change_file_content(
        &mut self,
        file: FileId,
        del_range: Option<TextRange>,
        ins_text: &str,
    ) -> Result<()> {
        let new_text = match del_range {
            None => ins_text.to_owned(),
            Some(del_range) => {
                let text = &*self.files[file.0 as usize].0;
                ensure!(
                    del_range.end() <= TextSize::of(text),
                    "Invalid delete range {del_range:?}",
                );
                let mut buf = String::with_capacity(
                    text.len() - usize::from(del_range.len()) + ins_text.len(),
                );
                buf += &text[..usize::from(del_range.start())];
                buf += ins_text;
                buf += &text[usize::from(del_range.end())..];
                buf
            }
        };
        // This is not quite efficient, but we already do many O(n) traversals.
        let (new_text, line_map) = LineMap::normalize(new_text);
        let new_text = <Arc<str>>::from(new_text);
        log::trace!("File {:?} content changed: {:?}", file, new_text);
        self.files[file.0 as usize] = (new_text.clone(), Arc::new(line_map));
        self.change.change_file(file, new_text);
        Ok(())
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Result<FileId> {
        self.local_file_set
            .file_for_path(path)
            .with_context(|| format!("File not loaded: {path:?}"))
    }

    pub fn file_for_uri(&self, uri: &Url) -> Result<FileId> {
        self.file_for_path(&uri.to_vfs_path())
    }

    pub fn uri_for_file(&self, file: FileId) -> Url {
        let vpath = self.local_file_set.path_for_file(file);
        Url::from_vfs_path(vpath)
    }

    pub fn take_change(&mut self) -> Change {
        let mut change = mem::take(&mut self.change);
        if mem::take(&mut self.root_changed) {
            change.set_roots(vec![SourceRoot::new_local(
                self.local_file_set.clone(),
                // TODO: Entry.
                None,
            )]);
        }
        change
    }

    pub fn content_for_file(&self, file: FileId) -> Arc<str> {
        self.files[file.0 as usize].0.clone()
    }

    pub fn line_map_for_file(&self, file: FileId) -> Arc<LineMap> {
        self.files[file.0 as usize].1.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LineMap {
    /// Invariant:
    /// - Have at least two elements.
    /// - The first must be 0.
    /// - The last must be the length of original text.
    line_starts: Vec<u32>,
    char_diffs: HashMap<u32, Vec<(u32, CodeUnitsDiff)>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CodeUnitsDiff {
    One = 1,
    Two = 2,
}

impl LineMap {
    fn normalize(mut text: String) -> (String, Self) {
        // Must be valid for `TextSize`.
        u32::try_from(text.len()).expect("Text too long");

        text.retain(|c| c != '\r');
        let bytes = text.as_bytes();

        let mut line_starts = Some(0)
            .into_iter()
            .chain(
                bytes
                    .iter()
                    .zip(0u32..)
                    .filter(|(b, _)| **b == b'\n')
                    .map(|(_, i)| i + 1),
            )
            .collect::<Vec<_>>();
        line_starts.push(text.len() as u32);

        let mut char_diffs = HashMap::new();
        for ((&start, &end), i) in line_starts.iter().zip(&line_starts[1..]).zip(0u32..) {
            let mut diffs = Vec::new();
            for (&b, pos) in bytes[start as usize..end as usize].iter().zip(0u32..) {
                let diff = match b {
                    0b0000_0000..=0b0111_1111 |                      // utf8_len == 1, utf16_len == 1
                    0b1000_0000..=0b1011_1111 => continue,           // Continuation bytes.
                    0b1100_0000..=0b1101_1111 => CodeUnitsDiff::One, // utf8_len == 2, utf16_len == 1
                    0b1110_0000..=0b1110_1111 => CodeUnitsDiff::Two, // utf8_len == 3, utf16_len == 1
                    0b1111_0000.. => CodeUnitsDiff::Two,             // utf8_len == 4, utf16_len == 2
                };
                diffs.push((pos, diff));
            }
            if !diffs.is_empty() {
                char_diffs.insert(i, diffs);
            }
        }

        let this = Self {
            line_starts,
            char_diffs,
        };
        (text, this)
    }

    pub fn last_line(&self) -> u32 {
        self.line_starts.len() as u32 - 2
    }

    pub fn pos_for_line_col(&self, line: u32, mut col: u32) -> TextSize {
        let pos = self.line_starts.get(line as usize).copied().unwrap_or(0);
        if let Some(diffs) = self.char_diffs.get(&line) {
            for &(char_pos, diff) in diffs {
                if char_pos < col {
                    col += diff as u32;
                }
            }
        }
        (pos + col).into()
    }

    pub fn line_col_for_pos(&self, pos: TextSize) -> (u32, u32) {
        let pos = u32::from(pos);
        let line = self
            .line_starts
            .partition_point(|&i| i <= pos)
            .saturating_sub(1);
        let mut col = pos - self.line_starts[line];
        if let Some(diffs) = self.char_diffs.get(&(line as u32)) {
            col -= diffs
                .iter()
                .take_while(|(char_pos, _)| *char_pos < col)
                .map(|(_, diff)| *diff as u32)
                .sum::<u32>();
        }
        (line as u32, col)
    }

    pub fn end_col_for_line(&self, line: u32) -> u32 {
        let mut len = self.line_starts[line as usize + 1] - self.line_starts[line as usize];
        if let Some(diffs) = self.char_diffs.get(&line) {
            len -= diffs.iter().map(|&(_, diff)| diff as u32).sum::<u32>();
        }
        // Lines except the last one has a trailing `\n`.
        // Note that `line_starts` has one element more than actual total lines.
        if line + 1 + 1 != self.line_starts.len() as u32 {
            len -= 1;
        }
        len
    }
}

#[cfg(test)]
mod tests {
    use super::{CodeUnitsDiff, LineMap};
    use std::collections::HashMap;

    #[test]
    fn line_map_ascii() {
        let s = "hello\nworld\nend";
        let (norm, map) = LineMap::normalize(s.into());
        assert_eq!(norm, s);
        assert_eq!(&map.line_starts, &[0, 6, 12, 15]);

        let mapping = [
            (0, 0, 0),
            (2, 0, 2),
            (5, 0, 5),
            (6, 1, 0),
            (11, 1, 5),
            (12, 2, 0),
        ];
        for (pos, line, col) in mapping {
            assert_eq!(map.line_col_for_pos(pos.into()), (line, col));
            assert_eq!(map.pos_for_line_col(line, col), pos.into());
        }
    }

    #[test]
    fn line_map_unicode() {
        //    |         | UTF-8       | UTF-16
        // A  | U+00041 | 41          | 0041
        // ß  | U+000DF | C3 9F       | 00DF
        // ℝ  | U+0211D | E2 84 9D    | 211D
        // 💣 | U+1F4A3 | F0 9F 92 A3 | D83D DCA3
        let s = "_A_ß_ℝ_💣_";
        let (norm, map) = LineMap::normalize(s.into());
        assert_eq!(norm, s);
        assert_eq!(&map.line_starts, &[0, 15]);
        assert_eq!(
            &map.char_diffs,
            &HashMap::from([(
                0u32,
                vec![
                    (3u32, CodeUnitsDiff::One),
                    (6, CodeUnitsDiff::Two),
                    (10, CodeUnitsDiff::Two),
                ],
            )])
        );

        let mapping = [
            (0, 0, 0),
            (1, 0, 1),
            (2, 0, 2),
            (3, 0, 3),
            (5, 0, 4),
            (6, 0, 5),
            (9, 0, 6),
            (10, 0, 7),
            (14, 0, 9),
        ];
        for (pos, line, col) in mapping {
            assert_eq!(map.line_col_for_pos(pos.into()), (line, col));
            assert_eq!(map.pos_for_line_col(line, col), pos.into());
        }
    }

    #[test]
    fn last_line() {
        let (_, map) = LineMap::normalize("".into());
        assert_eq!(map.last_line(), 0);
        let (_, map) = LineMap::normalize("\n".into());
        assert_eq!(map.last_line(), 1);
        let (_, map) = LineMap::normalize("foo\nbar".into());
        assert_eq!(map.last_line(), 1);
        let (_, map) = LineMap::normalize("foo\nbar\n".into());
        assert_eq!(map.last_line(), 2);
    }

    #[test]
    fn line_end_col() {
        // See comments in `line_map_unicode`.
        let (_, map) = LineMap::normalize("hello\nAßℝ💣\n\nend".into());
        assert_eq!(map.end_col_for_line(0), 5);
        assert_eq!(map.end_col_for_line(1), 5);
        assert_eq!(map.end_col_for_line(2), 0);
        assert_eq!(map.end_col_for_line(3), 3);
    }
}
