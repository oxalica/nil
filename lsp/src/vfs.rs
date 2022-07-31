use indexmap::IndexMap;
use lsp_types::Url;
use nil::{Change, FileId, FilePos};
use std::collections::HashMap;
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

#[derive(Default, Debug, PartialEq, Eq)]
struct LineMap {
    line_starts: Vec<u32>,
    char_diffs: HashMap<u32, Vec<(u32, CodeUnitsDiff)>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CodeUnitsDiff {
    One = 1,
    Two = 2,
}

impl LineMap {
    fn normalize(text: String) -> Option<(String, Self)> {
        // Too large for `TextSize`.
        if text.len() > u32::MAX as usize {
            return None;
        }

        let text = text.replace('\r', "");
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
        Some((text, this))
    }

    fn pos(&self, line: u32, mut col: u32) -> TextSize {
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

    fn line_col(&self, pos: TextSize) -> (u32, u32) {
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
}

#[cfg(test)]
mod tests {
    use super::{CodeUnitsDiff, LineMap};
    use std::collections::HashMap;

    #[test]
    fn line_map_ascii() {
        let (s, map) = LineMap::normalize("hello\nworld\nend".into()).unwrap();
        assert_eq!(s, "hello\nworld\nend");
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
            assert_eq!(map.line_col(pos.into()), (line, col));
            assert_eq!(map.pos(line, col), pos.into());
        }
    }

    #[test]
    fn line_map_unicode() {
        let (s, map) = LineMap::normalize("_A_ß_ℝ_💣_".into()).unwrap();
        assert_eq!(s, "_A_ß_ℝ_💣_");
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
            assert_eq!(map.line_col(pos.into()), (line, col));
            assert_eq!(map.pos(line, col), pos.into());
        }
    }
}
