use crate::Result;
use ide::{Change, FileId, FileSet, SourceRoot, VfsPath};
use lsp_types::Url;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::{fmt, mem};
use text_size::TextSize;

pub struct Vfs {
    // FIXME: Currently this list is append-only.
    files: Vec<Option<(Arc<str>, LineMap)>>,
    /// The root directory, which must be absolute.
    local_root: PathBuf,
    local_file_set: FileSet,
    root_changed: bool,
    change: Change,
}

impl fmt::Debug for Vfs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Vfs")
            .field("file_cnt", &self.files.len())
            .field("local_root", &self.local_root)
            .finish_non_exhaustive()
    }
}

impl Vfs {
    pub fn new(local_root: PathBuf) -> Self {
        assert!(local_root.is_absolute());
        Self {
            files: Vec::new(),
            local_root,
            local_file_set: FileSet::default(),
            root_changed: false,
            change: Change::default(),
        }
    }

    fn alloc_file_id(&mut self) -> FileId {
        let id = u32::try_from(self.files.len()).expect("Length overflow");
        self.files.push(None);
        FileId(id)
    }

    fn uri_to_vpath(&self, uri: &Url) -> Result<VfsPath> {
        let path = uri
            .to_file_path()
            .map_err(|_| format!("Non-file URI: {}", uri))?;
        let relative_path = path
            .strip_prefix(&self.local_root)
            .map_err(|_| format!("URI outside workspace: {}", uri))?;
        Ok(VfsPath::from_path(relative_path).expect("URI is UTF-8"))
    }

    pub fn set_uri_content(&mut self, uri: &Url, text: Option<String>) -> Result<()> {
        let vpath = self.uri_to_vpath(uri)?;
        self.set_path_content(vpath, text);
        Ok(())
    }

    pub fn set_path_content(&mut self, path: VfsPath, text: Option<String>) -> Option<FileId> {
        let content = text.and_then(LineMap::normalize);
        let (file, (text, line_map)) = match (self.local_file_set.get_file_for_path(&path), content)
        {
            (Some(file), None) => {
                self.local_file_set.remove_file(file);
                self.root_changed = true;
                self.files[file.0 as usize] = None;
                return None;
            }
            (None, None) => return None,
            (Some(file), Some(content)) => (file, content),
            (None, Some(content)) => {
                let file = self.alloc_file_id();
                self.local_file_set.insert(file, path);
                self.root_changed = true;
                (file, content)
            }
        };
        let text = <Arc<str>>::from(text);
        self.change.change_file(file, Some(text.clone()));
        self.files[file.0 as usize] = Some((text, line_map));
        Some(file)
    }

    pub fn get_file_for_uri(&self, uri: &Url) -> Result<FileId> {
        let vpath = self.uri_to_vpath(uri)?;
        self.local_file_set
            .get_file_for_path(&vpath)
            .ok_or_else(|| format!("URI not found: {}", uri).into())
    }

    pub fn uri_for_file(&self, file: FileId) -> Url {
        let vpath = self.local_file_set.path_for_file(file).as_str();
        assert!(!vpath.is_empty(), "Root is a directory");
        let path = self.local_root.join(vpath.strip_prefix('/').unwrap());
        Url::from_file_path(path).expect("Root is absolute")
    }

    pub fn take_change(&mut self) -> Change {
        let mut change = mem::take(&mut self.change);
        if self.root_changed {
            self.root_changed = false;
            // TODO: Configurable.
            let entry = ["/flake.nix", "/default.nix"].iter().find_map(|&path| {
                let path = VfsPath::new(path).unwrap();
                self.local_file_set.get_file_for_path(&path)
            });
            change.set_roots(vec![SourceRoot::new_local(
                self.local_file_set.clone(),
                entry,
            )]);
        }
        change
    }

    pub fn file_line_map(&self, file_id: FileId) -> &LineMap {
        &self.files[file_id.0 as usize]
            .as_ref()
            .expect("File must be valid")
            .1
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct LineMap {
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

    pub fn line_count(&self) -> u32 {
        self.line_starts.len() as u32 - 1
    }

    pub fn pos(&self, line: u32, mut col: u32) -> TextSize {
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

    pub fn line_col(&self, pos: TextSize) -> (u32, u32) {
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

    pub fn line_end_col(&self, line: u32) -> u32 {
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
        let (norm, map) = LineMap::normalize(s.into()).unwrap();
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
            assert_eq!(map.line_col(pos.into()), (line, col));
            assert_eq!(map.pos(line, col), pos.into());
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
        let (norm, map) = LineMap::normalize(s.into()).unwrap();
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
            assert_eq!(map.line_col(pos.into()), (line, col));
            assert_eq!(map.pos(line, col), pos.into());
        }
    }

    #[test]
    fn line_count() {
        let (_, map) = LineMap::normalize("".into()).unwrap();
        assert_eq!(map.line_count(), 1);
        let (_, map) = LineMap::normalize("\n".into()).unwrap();
        assert_eq!(map.line_count(), 2);
        let (_, map) = LineMap::normalize("foo\nbar".into()).unwrap();
        assert_eq!(map.line_count(), 2);
        let (_, map) = LineMap::normalize("foo\nbar\n".into()).unwrap();
        assert_eq!(map.line_count(), 3);
    }

    #[test]
    fn line_end_col() {
        // See comments in `line_map_unicode`.
        let (_, map) = LineMap::normalize("hello\nAßℝ💣\n\nend".into()).unwrap();
        assert_eq!(map.line_end_col(0), 5);
        assert_eq!(map.line_end_col(1), 5);
        assert_eq!(map.line_end_col(2), 0);
        assert_eq!(map.line_end_col(3), 3);
    }
}
