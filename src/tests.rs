use crate::base::SourceDatabaseStorage;
use crate::def::DefDatabaseStorage;
use crate::{Change, DefDatabase, FileId, FileSet, SourceRoot, VfsPath};
use indexmap::IndexMap;
use rowan::ast::AstNode;
use rowan::TextSize;
use std::{mem, ops};
use syntax::{NixLanguage, SyntaxNode};

pub const MARKER_INDICATOR: char = '$';

#[salsa::database(SourceDatabaseStorage, DefDatabaseStorage)]
#[derive(Default)]
pub struct TestDB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDB {}

impl TestDB {
    pub fn single_file<const MARKERS: usize>(
        fixture: &str,
    ) -> Result<(Self, FileId, [TextSize; MARKERS]), String> {
        let (f, file, poses) = Fixture::single_file(fixture)?;
        let db = Self::from_fixture(f);
        Ok((db, file, poses))
    }

    pub fn file_set(fixture: &str) -> Result<(Self, Vec<FileId>), String> {
        let f = Fixture::file_set(fixture)?;
        let files = (0..f.files.len())
            .map(|i| FileId(i as u32))
            .collect::<Vec<_>>();
        let db = Self::from_fixture(f);
        Ok((db, files))
    }

    fn from_fixture(fixture: Fixture) -> Self {
        let mut db = Self::default();
        let mut change = Change::new();
        let mut file_set = FileSet::default();
        for (i, (path, text)) in (0u32..).zip(fixture.files) {
            let file = FileId(i);
            file_set.insert(file, path);
            change.change_file(file, Some(text.into()));
        }
        change.set_roots(vec![SourceRoot::new_local(file_set)]);
        change.apply(&mut db);
        db
    }

    pub fn find_node<T>(
        &self,
        file_id: FileId,
        pos: TextSize,
        f: impl FnMut(SyntaxNode) -> Option<T>,
    ) -> Option<T> {
        self.parse(file_id)
            .syntax_node()
            .token_at_offset(pos)
            .right_biased()?
            .parent_ancestors()
            .find_map(f)
    }

    pub fn node_at<N: AstNode<Language = NixLanguage>>(
        &self,
        file_id: FileId,
        pos: TextSize,
    ) -> Option<N> {
        self.find_node(file_id, pos, N::cast)
    }
}

#[derive(Default, Debug)]
struct Fixture {
    files: IndexMap<VfsPath, String>,
}

impl ops::Index<FileId> for Fixture {
    type Output = str;
    fn index(&self, index: FileId) -> &Self::Output {
        &self.files[index.0 as usize]
    }
}

impl Fixture {
    fn insert_file(&mut self, path: VfsPath, text: String) -> Result<(), String> {
        if self.files.insert(path, text).is_none() {
            Ok(())
        } else {
            Err("Duplicated path".into())
        }
    }

    fn single_file<const MARKERS: usize>(
        fixture: &str,
    ) -> Result<(Self, FileId, [TextSize; MARKERS]), String> {
        if fixture.len() >= u32::MAX as usize {
            return Err("Size too large".into());
        }
        let mut markers = [TextSize::from(!0u32); MARKERS];
        let mut text = String::new();
        let mut chars = fixture.chars().peekable();
        while let Some(c) = chars.next() {
            if c == MARKER_INDICATOR {
                if let Some(n @ '0'..='9') = chars.peek().copied() {
                    chars.next();
                    let i = n.to_digit(10).unwrap() as usize;
                    let place = markers
                        .get_mut(i)
                        .ok_or_else(|| format!("Marker {} out of bound", i))?;
                    if *place != TextSize::from(!0u32) {
                        return Err(format!("Marker {} redefined", i));
                    }
                    *place = TextSize::from(text.len() as u32);
                    continue;
                }
            }
            text.push(c);
        }
        for (i, &pos) in markers.iter().enumerate() {
            if pos == TextSize::from(!0u32) {
                return Err(format!("Marker {} not set", i));
            }
        }
        let mut this = Self::default();
        this.insert_file(VfsPath::new("/default.nix").unwrap(), text)?;
        Ok((this, FileId(0), markers))
    }

    fn file_set(fixture: &str) -> Result<Self, String> {
        if fixture.len() >= u32::MAX as usize {
            return Err("Size too large".into());
        }

        let mut this = Self::default();
        let mut cur_path = None;
        let mut cur_text = String::new();
        for line in fixture.lines().skip_while(|line| line.is_empty()) {
            if let Some(path) = line.strip_prefix("#- ") {
                let path = VfsPath::new(path).ok_or_else(|| "Invalid path".to_owned())?;
                if let Some(prev_path) = cur_path.replace(path) {
                    this.insert_file(prev_path, mem::take(&mut cur_text))?;
                }
            } else {
                if cur_path.is_none() {
                    return Err("No path specified".into());
                }
                cur_text += line;
                cur_text += "\n";
            }
        }
        this.insert_file(
            cur_path.ok_or_else(|| "Empty fixture".to_owned())?,
            cur_text,
        )?;
        Ok(this)
    }
}
