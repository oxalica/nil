use itertools::Itertools;
use syntax::{ast, match_ast, SyntaxKind, SyntaxToken, TextRange};

use crate::{DefDatabase, FileId};

#[derive(Debug, Clone)]
pub enum InlayHintKind {
    AttrsetAttribute(String),
}

#[derive(Debug, Clone)]
pub struct InlayHintResult {
    pub range: TextRange,
    pub kind: InlayHintKind,
}

pub(crate) fn inlay_hints(
    db: &dyn DefDatabase,
    file: FileId,
    range: Option<TextRange>,
) -> Vec<InlayHintResult> {
    let root_node = db.parse(file).syntax_node();

    let (first_tok, end_pos) = match range {
        None => (root_node.first_token(), u32::MAX.into()),
        Some(range) => (
            root_node.token_at_offset(range.start()).right_biased(),
            range.end(),
        ),
    };

    // TODO:
    // What is the best way to match ast?
    // It would be nice to match on the shape `{ ... = <expr> ; }`
    let hint_kind = |tok: &SyntaxToken| -> Option<InlayHintKind> {
        let prev_token = {
            let mut iter = std::iter::successors(Some(tok.clone()), |tok| tok.prev_token());
            iter.next();
            iter.find_or_first(|tok| !tok.kind().is_trivia())
        };
        let next_token = {
            let mut iter = std::iter::successors(Some(tok.clone()), |tok| tok.next_token());
            iter.next();
            iter.find_or_first(|tok| !tok.kind().is_trivia())
        };

        match (prev_token, next_token) {
            (Some(prev), Some(next))
                if prev.kind() == SyntaxKind::EQ && next.kind() == SyntaxKind::SEMICOLON =>
            {
                // TODO: inlay hint label
                Some(InlayHintKind::AttrsetAttribute("text".into()))
            }
            _ => None,
        }
    };

    std::iter::successors(first_tok, |tok| tok.next_token())
        .take_while(|tok| tok.text_range().start() < end_pos)
        .filter(|tok| !tok.kind().is_trivia())
        .filter_map(|tok| {
            Some(InlayHintResult {
                range: tok.text_range(),
                kind: hint_kind(&tok)?,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use crate::{DefDatabase, FilePos};
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let FilePos { file_id, .. } = f[0];
        assert_eq!(db.parse(file_id).errors(), &[]);

        let hints = super::inlay_hints(&db, file_id, None);
        eprintln!("{:?}", hints);
        expect.assert_eq("unimplemented");
    }

    #[test]
    fn hint() {
        check("$0{ foo = true; }", expect!["fail"]);
    }
}
