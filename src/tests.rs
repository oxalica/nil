use crate::base::{SourceDatabase, SourceDatabaseStorage};
use crate::def::DefDatabaseStorage;
use crate::{Change, FileId};
use rowan::ast::AstNode;
use rowan::TextSize;
use std::ops;
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

    fn from_fixture(fixture: Fixture) -> Self {
        let mut db = Self::default();
        let root = FileId(0);
        let mut change = Change::new();
        change.change_file(root, Some(fixture[root].into()));
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

#[derive(Debug)]
struct Fixture {
    texts: Vec<String>,
}

impl ops::Index<FileId> for Fixture {
    type Output = str;
    fn index(&self, index: FileId) -> &Self::Output {
        &self.texts[index.0 as usize]
    }
}

impl Fixture {
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
        Ok((Self { texts: vec![text] }, FileId(0), markers))
    }
}
