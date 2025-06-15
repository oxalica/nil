use crate::{DefDatabase, FileId};
use itertools::Itertools;
use std::fmt::Display;
use std::num::NonZero;
use syntax::ast::{self, AstNode};
use syntax::{SyntaxKind, SyntaxToken, TextRange};

#[derive(Debug, Clone)]
pub enum InlayHintKind {
    LetBindingEnd(String),
    AttrsetBindingEnd(String),
    RecAttrsetBindingEnd(String),
}

impl Display for InlayHintKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LetBindingEnd(s) => write!(f, "= let {s}"),
            Self::AttrsetBindingEnd(s) => write!(f, "= {s}"),
            Self::RecAttrsetBindingEnd(s) => write!(f, "= rec {s}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InlayHintResult {
    pub range: TextRange,
    pub kind: InlayHintKind,
}

#[derive(Debug, Clone)]
pub struct InlayHintsConfig {
    pub binding_end_hints_min_lines: Option<NonZero<usize>>,
}

pub(crate) fn inlay_hints(
    db: &dyn DefDatabase,
    file: FileId,
    range: Option<TextRange>,
    config: InlayHintsConfig,
) -> Vec<InlayHintResult> {
    let Some(threshold) = config.binding_end_hints_min_lines else {
        return vec![];
    };

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
            let attr_path_value = tok.parent()?;

            let spaning_lines = {
                let mut acc = 1_usize; // we count from 1
                attr_path_value
                    .text()
                    .for_each_chunk(|s| acc += s.matches('\n').count());
                NonZero::new(acc).expect("must have positive amount of lines")
            };
            if spaning_lines < threshold {
                return None;
            }

            let is_rec_attr = ast::AttrSet::cast(attr_path_value.parent()?)
                .map(|attr| attr.rec_token())
                .flatten()
                .is_some();
            let is_let = ast::LetIn::cast(attr_path_value.parent()?).is_some();

            let mut attr_path = attr_path_value
                .first_child_by_kind(&|u: SyntaxKind| u == SyntaxKind::ATTR_PATH)?
                .children()
                .filter(|node| !node.kind().is_trivia());

            let attr_name = attr_path.join(".");

            if is_let {
                Some(InlayHintKind::LetBindingEnd(attr_name))
            } else if is_rec_attr {
                Some(InlayHintKind::RecAttrsetBindingEnd(attr_name))
            } else {
                Some(InlayHintKind::AttrsetBindingEnd(attr_name))
            }
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
    use crate::ide::inlay_hints::InlayHintsConfig;
    use crate::tests::TestDB;
    use crate::{DefDatabase, FilePos};
    use expect_test::{expect, Expect};
    use itertools::Itertools;
    use std::num::NonZero;

    fn check(fixture: &str, expect: Expect, config: InlayHintsConfig) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let FilePos { file_id, .. } = f[0];
        assert_eq!(db.parse(file_id).errors(), &[]);

        let hints = super::inlay_hints(&db, file_id, None, config)
            .into_iter()
            .map(|hint| hint.kind)
            .join(",");

        expect.assert_eq(&hints);
    }

    #[track_caller]
    fn check_1(fixture: &str, expect: Expect) {
        check(
            fixture,
            expect,
            InlayHintsConfig {
                binding_end_hints_min_lines: Some(NonZero::new(1).expect("1 is nonzero")),
            },
        )
    }

    #[track_caller]
    fn check_none(fixture: &str, expect: Expect) {
        check(
            fixture,
            expect,
            InlayHintsConfig {
                binding_end_hints_min_lines: None,
            },
        )
    }

    #[track_caller]
    fn check_5(fixture: &str, expect: Expect) {
        check(
            fixture,
            expect,
            InlayHintsConfig {
                binding_end_hints_min_lines: Some(NonZero::new(5).expect("5 is nonzero")),
            },
        )
    }

    #[test]
    fn attrset_hint() {
        check_1("$0{ foo = true; }", expect!["= foo"]);
        check_1("$0{ foo = [true true true]; }", expect!["= foo"]);
        check_1(
            "$0{ foo.bar = { baz = [true true true]; }; }",
            expect!["= baz,= foo.bar"],
        );
        check_none(
            r"
            $0{
                foo =
                #
                #
                #
                #
                #
                true;
            }
            ",
            expect![],
        );
        check_5(
            r"
            $0{
                foo =
                #
                true;
            }
            ",
            expect![],
        );
        check_5(
            r"
            $0{
                foo =
                #
                #
                #
                true;
            }
            ",
            expect!["= foo"],
        );
    }

    #[test]
    fn rec_attrset_hint() {
        check_1("$0rec { foo = true; }", expect!["= rec foo"]);
        check_1("$0rec{ foo = [true true true]; }", expect!["= rec foo"]);
        check_1(
            "$0{ foo.bar = rec{ baz = [true true true]; }; }",
            expect!["= rec baz,= foo.bar"],
        );
    }

    #[test]
    fn let_hint() {
        check_1("$0let foo = true; in null", expect!["= let foo"]);
        check_1("$0let foo = [true true true]; in {}", expect!["= let foo"]);
    }
}
