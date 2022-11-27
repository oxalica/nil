use crate::def::{AstPtr, ResolveResult};
use crate::{DefDatabase, FilePos};
use syntax::ast::{self, AstNode};
use syntax::{best_token_at_offset, TextRange, T};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HlRelated {
    pub range: TextRange,
    pub is_definition: bool,
}

pub(crate) fn highlight_related(db: &dyn DefDatabase, fpos: FilePos) -> Option<Vec<HlRelated>> {
    let parse = db.parse(fpos.file_id);
    let source_map = db.source_map(fpos.file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), fpos.pos)?;

    // For `with` token, highlight itself and all Attr references.
    if tok.kind() == T![with] {
        let with_node = ast::With::cast(tok.parent()?)?;
        let with_expr = source_map.expr_for_node(AstPtr::new(with_node.syntax()))?;
        let nameref = db.name_reference(fpos.file_id);
        return Some(
            // Also include the current token.
            std::iter::once(HlRelated {
                range: tok.text_range(),
                is_definition: true,
            })
            .chain(
                nameref
                    .with_references(with_expr)
                    .unwrap_or(&[])
                    .iter()
                    .flat_map(|&e| {
                        Some(HlRelated {
                            range: source_map.node_for_expr(e)?.text_range(),
                            is_definition: false,
                        })
                    }),
            )
            .collect(),
        );
    }

    // Resolve the definition `Name` for `Attr` and `Ref`.
    // N.B. We prefer Expr than Name, mainly for highlighting the outer definition of
    // `inherit`ed Attrs. This is covered by `tests::inherit`.
    let name = if let Some((ref_node, ref_expr)) = tok.parent().and_then(|ref_node| {
        let ref_expr = source_map.expr_for_node(AstPtr::new(&ref_node))?;
        Some((ref_node, ref_expr))
    }) {
        match db.name_resolution(fpos.file_id).get(ref_expr)? {
            ResolveResult::Definition(name) => *name,
            ResolveResult::Builtin(_) => return None,
            // We highlight all effective `with` as definitions and
            // all other Attr references of the innermost `with`.
            ResolveResult::WithExprs(with_exprs) => {
                return Some(
                    with_exprs
                        .iter()
                        .filter_map(|&e| {
                            let ptr = source_map.node_for_expr(e)?;
                            let with_node = ast::With::cast(ptr.to_node(&parse.syntax_node()))?;
                            Some(HlRelated {
                                range: with_node.with_token()?.text_range(),
                                is_definition: true,
                            })
                        })
                        // Also include the current token.
                        .chain(Some(HlRelated {
                            range: ref_node.text_range(),
                            is_definition: false,
                        }))
                        .collect(),
                );
            }
        }
    } else if let Some(attr_node) = tok.parent().and_then(ast::Attr::cast) {
        let attr_ptr = AstPtr::new(attr_node.syntax());
        source_map.name_for_node(attr_ptr)?
    } else {
        return None;
    };

    let mut ret = Vec::new();

    // Definitions.
    ret.extend(source_map.nodes_for_name(name).map(|ptr| HlRelated {
        range: ptr.text_range(),
        is_definition: true,
    }));

    // References.
    ret.extend(
        db.name_reference(fpos.file_id)
            .name_references(name)
            .into_iter()
            .flatten()
            .filter_map(|&e| {
                Some(HlRelated {
                    range: source_map.node_for_expr(e)?.text_range(),
                    is_definition: false,
                })
            }),
    );

    Some(ret)
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use crate::SourceDatabase;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        let mut hls = super::highlight_related(&db, f[0]).unwrap_or_default();
        hls.sort_by_key(|hl| hl.range.start());
        assert!(!hls.is_empty(), "No highlights");

        let mut src = db.file_content(f[0].file_id).to_string();
        for hl in hls.iter().rev() {
            let (open, close) = if hl.is_definition {
                ("<<", ">>")
            } else {
                ("<", ">")
            };
            src.insert_str(usize::from(hl.range.end()), close);
            src.insert_str(usize::from(hl.range.start()), open);
        }
        expect.assert_eq(&src);
    }

    #[test]
    fn definition() {
        check("$0a: a + (a: a)", expect!["<<a>>: <a> + (a: a)"]);
        check(
            "let $0a.a = 1; a.b = 2; in a.a",
            expect!["let <<a>>.a = 1; <<a>>.b = 2; in <a>.a"],
        );
        check(
            "let b.$0a.a = 1; b.a = { }; a = 1; in b.a",
            expect!["let b.<<a>>.a = 1; b.<<a>> = { }; a = 1; in b.a"],
        );
    }

    #[test]
    fn reference() {
        check("a: $0a + (a: a)", expect!["<<a>>: <a> + (a: a)"]);
        check(
            "let a.a = 1; a.b = 2; in $0a.a",
            expect!["let <<a>>.a = 1; <<a>>.b = 2; in <a>.a"],
        );
    }

    #[test]
    fn inherit() {
        check(
            "let $0a = 1; in { inherit a; }",
            expect!["let <<a>> = 1; in { inherit <a>; }"],
        );
        check(
            "let a = 1; in { inherit $0a; }",
            expect!["let <<a>> = 1; in { inherit <a>; }"],
        );
        check(
            "let $0a = 1; in rec { inherit a; }",
            expect!["let <<a>> = 1; in rec { inherit <a>; }"],
        );
        check(
            "let a = 1; in rec { inherit $0a; }",
            expect!["let <<a>> = 1; in rec { inherit <a>; }"],
        );
    }

    #[test]
    fn with() {
        check(
            "with 1; a + (with 2; $0a + b (with 3; a))",
            expect!["<<with>> 1; a + (<<with>> 2; <a> + b (with 3; a))"],
        );
        check(
            "with 1; a + ($0with 2; a + b (with 3; a))",
            expect!["with 1; a + (<<with>> 2; <a> + <b> (with 3; <a>))"],
        );
    }
}
