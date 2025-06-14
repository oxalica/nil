use itertools::Itertools;
use syntax::ast::{self, AstNode};
use syntax::{match_ast, NixLanguage, SyntaxKind, SyntaxToken, TextRange};

use crate::ide::expand_selection::expand_selection;
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

    let hint_kind = |tok: &SyntaxToken| -> Option<InlayHintKind> {
        if tok.kind() == SyntaxKind::SEMICOLON {
            let attribute_node = tok.parent()?;

            let attrname_node = attribute_node
                .first_child_by_kind(&|u: SyntaxKind| u == SyntaxKind::ATTR_PATH)?
                .children()
                .filter(|node| !node.kind().is_trivia());

            // HACK: detect trivial when we can not show the hint
            if attribute_node.text_range().len() < 200.into() {
                return None;
            }

            let attr_name = attrname_node.map(|node| node.to_string()).join(".");
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
                range: tok.text_range(),
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
        check("$0{ foo = true; }", expect![]);
        check("$0{ foo = [true true true]; }", expect![]);
        check(
            r"
                $0{
                    foo.bar = {
                        _0 = true;
                        _1 = true;
                        _2 = true;
                        _3 = true;
                        _4 = true;
                        _5 = true;
                        _6 = true;
                        _7 = true;
                        _8 = true;
                        _9 = true;
                        _10 = true;
                        _11 = true;
                        baz = [true true true];
                    };
                }
            ",
            expect!["foo.bar"],
        );
    }
}
