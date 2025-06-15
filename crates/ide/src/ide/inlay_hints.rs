use crate::{DefDatabase, FileId};
use itertools::Itertools;
use syntax::{SyntaxKind, SyntaxToken, TextRange};

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

    let hint_kind = |tok: &SyntaxToken| -> Option<InlayHintKind> {
        if tok.kind() == SyntaxKind::SEMICOLON {
            let mut attribute_node = tok
                .parent()?
                .first_child_by_kind(&|u: SyntaxKind| u == SyntaxKind::ATTR_PATH)?
                .children()
                .filter(|node| !node.kind().is_trivia());

            let attr_name = attribute_node.join(".");
            Some(InlayHintKind::AttrsetAttribute(attr_name))
        } else {
            None
        }
    };

    std::iter::successors(first_tok, |tok| tok.next_token())
        .take_while(|tok| tok.text_range().start() < end_pos)
        .filter(|tok| !tok.kind().is_trivia())
        .filter_map(|tok| {
            Some(InlayHintResult {
                range: tok.parent()?.text_range(),
                kind: hint_kind(&tok)?,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use crate::{DefDatabase, FilePos, InlayHintKind, InlayHintResult};
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let FilePos { file_id, .. } = f[0];
        assert_eq!(db.parse(file_id).errors(), &[]);

        let hints = super::inlay_hints(&db, file_id, None)
            .iter()
            .map(|hint| {
                let InlayHintResult { kind, .. } = hint;
                match kind {
                    InlayHintKind::AttrsetAttribute(label) => label.to_owned(),
                }
            })
            .collect::<Vec<_>>()
            .join(",");

        expect.assert_eq(&hints);
    }

    #[test]
    fn hint() {
        check("$0{ foo = true; }", expect!["foo"]);
        check("$0{ foo = [true true true]; }", expect!["foo"]);
        check(
            "$0{ foo.bar = { baz = [true true true]; }; }",
            expect!["baz,foo.bar"],
        );
    }
}
