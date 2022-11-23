use crate::{DefDatabase, FileRange};
use rowan::{NodeOrToken, TextRange};
use syntax::{best_token_at_offset, SyntaxKind, SyntaxNode, T};

/// Interesting parent ranges covering the given range.
/// Returns all ranges from the smallest to the largest.
pub(crate) fn expand_selection(
    db: &dyn DefDatabase,
    FileRange { file_id, range }: FileRange,
) -> Option<Vec<TextRange>> {
    let parse = db.parse(file_id);

    let mut ret = Vec::new();

    let leaf = if range.start() == range.end() {
        let tok = best_token_at_offset(&parse.syntax_node(), range.start())?;
        NodeOrToken::Token(tok)
    } else {
        parse.syntax_node().covering_element(range)
    };

    let node = match leaf {
        NodeOrToken::Node(node) => Some(node),
        NodeOrToken::Token(tok) => {
            if is_node_kind_good(tok.kind()) {
                ret.push(tok.text_range());
            }
            tok.parent()
        }
    };

    ret.extend(
        std::iter::successors(node, |node| node.parent()).filter_map(|node| {
            is_node_kind_good(node.kind())
                .then(|| non_space_range(&node))
                .flatten()
        }),
    );
    ret.dedup();

    Some(ret)
}

/// Trim spaces for the range of a node.
/// Note that comments are not trimmed.
fn non_space_range(node: &SyntaxNode) -> Option<TextRange> {
    // Whitespaces would be merged into one token if exist.
    let first = node.first_token()?;
    let lhs = if first.kind() != SyntaxKind::SPACE {
        first.text_range().start()
    } else {
        first.text_range().end()
    };

    let last = node.last_token()?;
    let rhs = if last.kind() != SyntaxKind::SPACE {
        last.text_range().end()
    } else {
        last.text_range().start()
    };

    Some(TextRange::empty(lhs).cover_offset(rhs))
}

/// If this node/token kind is good enough to show as interesting selection.
fn is_node_kind_good(kind: SyntaxKind) -> bool {
    !matches!(
        kind,
        // Ignore spaces, but keep comments.
        | SyntaxKind::SPACE
        // Ignore fragments and internal tokens.
        | SyntaxKind::STRING_FRAGMENT
        | SyntaxKind::PATH_FRAGMENT
        | SyntaxKind::PATH_START
        | SyntaxKind::PATH_END
        // Ignore delimiters. Only select the whole node.
        | T!['('] | T![')']
        | T!['['] | T![']']
        | T!['{'] | T!['}'] | T!["${"]
        | T!['"'] | T!["''"]
        // Separators seem not useful.
        | T![,] | T![;] | T![.]
    )
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let frange = f.marker_single_range();
        let src = db.file_content(f[0].file_id);
        let got = super::expand_selection(&db, frange)
            .into_iter()
            .flatten()
            .flat_map(|range| {
                assert_eq!(src[range].trim(), &src[range]);
                [&src[range], "\n"]
            })
            .collect::<String>();
        expect.assert_eq(&got);
    }

    #[test]
    fn operators() {
        check(
            "a + b $0c * d == e",
            expect![[r#"
                c
                b c
                b c * d
                a + b c * d
                a + b c * d == e
            "#]],
        );

        check(
            "a + b $0* c",
            expect![[r#"
                *
                b * c
                a + b * c
            "#]],
        );
    }

    #[test]
    fn comment() {
        check(
            "f /* $0foo */ x",
            expect![[r#"
                /* foo */
                f /* foo */ x
            "#]],
        );
    }

    #[test]
    fn binding_path_value() {
        check(
            "let a.b.c = a; $0in a",
            expect![[r#"
                in
                let a.b.c = a; in a
            "#]],
        );
        check(
            "let a.b.c = $0a; in a",
            expect![[r#"
                a
                a.b.c = a;
                let a.b.c = a; in a
            "#]],
        );
        check(
            "let a.b.c $0= a; in a",
            expect![[r#"
                =
                a.b.c = a;
                let a.b.c = a; in a
            "#]],
        );
        check(
            "let a.$0b.c = a; in a",
            expect![[r#"
                b
                a.b.c
                a.b.c = a;
                let a.b.c = a; in a
            "#]],
        );
        check(
            "let a$0.b.$1c = a; in a",
            expect![[r#"
                a.b.c
                a.b.c = a;
                let a.b.c = a; in a
            "#]],
        );
    }

    #[test]
    fn inherit() {
        check(
            "{ inherit a $0b; }",
            expect![[r#"
                b
                inherit a b;
                { inherit a b; }
            "#]],
        );
        check(
            "{ inherit$0 a b; }",
            expect![[r#"
                inherit
                inherit a b;
                { inherit a b; }
            "#]],
        );
        check(
            "{ inheri$0t a$1 b; }",
            expect![[r#"
                inherit a b;
                { inherit a b; }
            "#]],
        );

        check(
            "{ inherit (a$0) a b; }",
            expect![[r#"
                a
                (a)
                inherit (a) a b;
                { inherit (a) a b; }
            "#]],
        );
        check(
            "{ inherit (a)$0 a b; }",
            expect![[r#"
                (a)
                inherit (a) a b;
                { inherit (a) a b; }
            "#]],
        );
    }
}
