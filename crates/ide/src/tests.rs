use crate::base::SourceDatabaseStorage;
use crate::def::DefDatabaseStorage;
use crate::{Change, DefDatabase, FileId, FilePos, FileSet, SourceRoot, VfsPath};
use anyhow::{ensure, Context, Result};
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
    pub fn single_file(fixture: &str) -> Result<(Self, FileId)> {
        let (db, f) = Self::from_fixture(fixture)?;
        ensure!(f.files().len() == 1, "Fixture contains multiple files");
        let file_id = f.files()[0];
        Ok((db, file_id))
    }

    pub fn from_fixture(fixture: &str) -> Result<(Self, Fixture)> {
        let f = Fixture::new(fixture)?;
        let mut db = Self::default();
        let mut change = Change::new();
        let mut file_set = FileSet::default();
        for (i, (path, text)) in (0u32..).zip(&f.files) {
            let file = FileId(i);
            file_set.insert(file, path.clone());
            change.change_file(file, text.to_owned().into());
        }
        let entry = file_set
            .file_for_path(&VfsPath::new("/default.nix").unwrap())
            .context("Missing entry file")?;
        change.set_roots(vec![SourceRoot::new_local(file_set, Some(entry))]);
        change.apply(&mut db);
        Ok((db, f))
    }

    pub fn find_node<T>(&self, fpos: FilePos, f: impl FnMut(SyntaxNode) -> Option<T>) -> Option<T> {
        self.parse(fpos.file_id)
            .syntax_node()
            .token_at_offset(fpos.pos)
            .right_biased()?
            .parent_ancestors()
            .find_map(f)
    }

    pub fn node_at<N: AstNode<Language = NixLanguage>>(&self, fpos: FilePos) -> Option<N> {
        self.find_node(fpos, N::cast)
    }
}

#[derive(Default, Debug)]
pub struct Fixture {
    files: IndexMap<VfsPath, String>,
    file_ids: Vec<FileId>,
    markers: Vec<FilePos>,
}

impl ops::Index<usize> for Fixture {
    type Output = FilePos;
    fn index(&self, index: usize) -> &Self::Output {
        &self.markers[index]
    }
}

impl<'a> ops::Index<&'a str> for Fixture {
    type Output = FileId;
    fn index(&self, index: &'a str) -> &Self::Output {
        let id = self
            .files
            .get_index_of(&VfsPath::new(index).unwrap())
            .unwrap();
        &self.file_ids[id]
    }
}

impl Fixture {
    pub fn new(fixture: &str) -> Result<Self> {
        ensure!(
            u32::try_from(fixture.len()).is_ok(),
            "Size too large: {}",
            fixture.len()
        );

        let mut this = Self::default();
        let mut missing_header = false;
        let mut cur_path = None;
        let mut cur_text = String::new();
        let mut cur_file = FileId(0);
        let mut markers = [None; 10];
        for line in fixture.lines().skip_while(|line| line.is_empty()) {
            if let Some(path) = line.strip_prefix("#- ") {
                ensure!(!missing_header, "Missing path header at the first line");

                let path = VfsPath::new(path)?;
                if let Some(prev_path) = cur_path.replace(path) {
                    this.insert_file(prev_path, mem::take(&mut cur_text))?;
                    cur_file.0 += 1;
                }
            } else {
                if cur_path.is_none() {
                    missing_header = true;
                    cur_path = Some(VfsPath::new("/default.nix").unwrap());
                }

                let mut iter = line.chars().peekable();
                while let Some(ch) = iter.next() {
                    if ch == MARKER_INDICATOR
                        && matches!(iter.peek(), Some(c) if c.is_ascii_digit())
                    {
                        let n = iter.next().unwrap().to_digit(10).unwrap() as usize;
                        let pos =
                            FilePos::new(cur_file, TextSize::try_from(cur_text.len()).unwrap());
                        ensure!(
                            markers[n].replace(pos).is_none(),
                            "Duplicated marker: {}",
                            n
                        );
                    } else {
                        cur_text.push(ch);
                    }
                }
                cur_text += "\n";
            }
        }
        this.insert_file(cur_path.context("Empty fixture")?, cur_text)?;

        let marker_len = markers
            .iter()
            .rposition(|p| p.is_some())
            .map_or(0, |n| n + 1);
        this.markers = markers
            .into_iter()
            .take(marker_len)
            .enumerate()
            .map(|(i, p)| p.with_context(|| format!("Discontinuous marker: {}", i)))
            .collect::<Result<Vec<_>>>()?;

        Ok(this)
    }

    fn insert_file(&mut self, path: VfsPath, mut text: String) -> Result<()> {
        let file = FileId(self.files.len() as u32);
        text.truncate(text.trim_end().len());
        ensure!(
            self.files.insert(path.clone(), text).is_none(),
            "Duplicated path: {:?}",
            path
        );
        self.file_ids.push(file);
        Ok(())
    }

    pub fn files(&self) -> &[FileId] {
        &self.file_ids
    }

    pub fn markers(&self) -> &[FilePos] {
        &self.markers
    }
}
