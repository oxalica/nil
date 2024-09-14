//! This is actually so-called "semantic highlighting".
//! Ref: <https://github.com/rust-lang/rust-analyzer/blob/a670ff888437f4b6a3d24cc2996e9f969a87cbae/crates/ide/src/syntax_highlighting/tags.rs>
use crate::def::{AstPtr, Expr, Literal, NameKind, ResolveResult};
use crate::{DefDatabase, FileId};
use builtin::{BuiltinKind, ALL_BUILTINS};
use syntax::ast::AstNode;
use syntax::{ast, match_ast, SyntaxKind, SyntaxToken, TextRange, T};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HlRange {
    pub range: TextRange,
    pub tag: HlTag,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum HlTag {
    NameDef(NameKind),
    NameRef(NameKind),
    UnresolvedRef,

    AttrField(HlAttrField),
    Builtin(BuiltinKind),
    Comment,
    BoolLiteral,
    FloatLiteral,
    IntLiteral,
    Keyword(HlKeyword),
    Operator(HlOperator),
    Path,
    Punct(HlPunct),
    StringEscape,
    StringLiteral,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum HlAttrField {
    Select,
    With,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum HlKeyword {
    Conditional,
    Operator,
    Other,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum HlOperator {
    Logical,
    Comparison,
    Arithmetic,
    Aggregation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum HlPunct {
    Brace,
    Bracket,
    Paren,
    Dot,
    Question,
    Comma,
    Semicolon,
    Equal,
    Colon,
    At,
    Ellipsis,
}

pub(crate) fn highlight(
    db: &dyn DefDatabase,
    file: FileId,
    range: Option<TextRange>,
) -> Vec<HlRange> {
    let root_node = db.parse(file).syntax_node();
    let source_map = db.source_map(file);
    let nameres = db.name_resolution(file);
    let module = db.module(file);

    let ident_tag = |tok: &SyntaxToken| -> Option<HlTag> {
        match tok.parent() {
            Some(node) if node.kind() == SyntaxKind::REF => {
                let expr = source_map.expr_for_node(AstPtr::new(&node))?;
                if let Some(builtin) = nameres.check_builtin(expr, &module) {
                    // Special case boolean literals.
                    if matches!(builtin, "true" | "false") {
                        return Some(HlTag::BoolLiteral);
                    }
                    return Some(HlTag::Builtin(ALL_BUILTINS[builtin].kind));
                }
                Some(match nameres.get(expr) {
                    None => HlTag::UnresolvedRef,
                    Some(ResolveResult::Definition(def)) => HlTag::NameRef(module[*def].kind),
                    Some(ResolveResult::WithExprs(_)) => HlTag::AttrField(HlAttrField::With),
                    // Covered by `check_builtin`.
                    Some(ResolveResult::Builtin(_)) => unreachable!(),
                })
            }
            Some(node) if node.kind() == SyntaxKind::NAME => {
                let ptr = AstPtr::new(&node);
                match source_map.name_for_node(ptr) {
                    // Definition.
                    Some(name) => {
                        // `inherit (builtins) head;`
                        //                     ^^^^
                        if nameres.is_inherited_builtin(name) {
                            return Some(HlTag::Builtin(ALL_BUILTINS[&*module[name].text].kind));
                        }

                        Some(HlTag::NameDef(module[name].kind))
                    }
                    // Selection.
                    None => {
                        let expr = source_map.expr_for_node(ptr)?;
                        // Attrs in select-expression should be converted into string literals.
                        let Expr::Literal(Literal::String(attr_text)) = &module[expr] else {
                            return None;
                        };

                        let path_node = ast::Attrpath::cast(node.parent()?)?;
                        let set_node = match_ast! {
                            match (path_node.syntax().parent()?) {
                                ast::HasAttr(n) => n.set(),
                                ast::Select(n) => n.set(),
                                _ => None,
                            }
                        }?;
                        let set_expr = source_map.expr_for_node(AstPtr::new(set_node.syntax()))?;

                        // `builtins.xxx`
                        //           ^^^
                        if let Some(ResolveResult::Builtin("builtins")) = nameres.get(set_expr) {
                            if let Some(b) = ALL_BUILTINS.get(attr_text) {
                                return Some(HlTag::Builtin(b.kind));
                            }
                        }
                        Some(HlTag::AttrField(HlAttrField::Select))
                    }
                }
            }
            _ => None,
        }
    };

    let token_tag = |tok: &SyntaxToken| -> Option<HlTag> {
        Some(match tok.kind() {
            SyntaxKind::SPACE => return None,
            SyntaxKind::COMMENT => HlTag::Comment,
            SyntaxKind::PATH | SyntaxKind::SEARCH_PATH => HlTag::Path,
            SyntaxKind::FLOAT => HlTag::FloatLiteral,
            SyntaxKind::INT => HlTag::IntLiteral,
            T!["''"] | T!['"'] | SyntaxKind::URI => HlTag::StringLiteral,
            SyntaxKind::STRING_ESCAPE => HlTag::StringEscape,
            // Don't color the string content. They are subjects to injection of other languages.
            SyntaxKind::STRING_FRAGMENT => return None,

            T![&&] | T![||] | T![->] | T![!] => HlTag::Operator(HlOperator::Logical),
            T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=] => {
                HlTag::Operator(HlOperator::Comparison)
            }
            T![+] | T![-] | T![*] | T![/] => HlTag::Operator(HlOperator::Arithmetic),
            T![++] | T![|>] | T!["//"] => HlTag::Operator(HlOperator::Aggregation),
            T!['{'] | T!['}'] | T!["${"] => HlTag::Punct(HlPunct::Brace),
            T!['['] | T![']'] => HlTag::Punct(HlPunct::Bracket),
            T!['('] | T![')'] => HlTag::Punct(HlPunct::Paren),
            T![.] => HlTag::Punct(HlPunct::Dot),
            T![?] => HlTag::Punct(HlPunct::Question),
            T![,] => HlTag::Punct(HlPunct::Comma),
            T![;] => HlTag::Punct(HlPunct::Semicolon),
            T![=] => HlTag::Punct(HlPunct::Equal),
            T![:] => HlTag::Punct(HlPunct::Colon),
            T![@] => HlTag::Punct(HlPunct::At),
            T![...] => HlTag::Punct(HlPunct::Ellipsis),

            T![if] | T![then] | T![else] => HlTag::Keyword(HlKeyword::Conditional),
            T![or] => HlTag::Keyword(HlKeyword::Operator),
            T![assert] | T![in] | T![inherit] | T![let] | T![rec] | T![with] => {
                HlTag::Keyword(HlKeyword::Other)
            }

            SyntaxKind::IDENT => return ident_tag(tok),
            _ => return None,
        })
    };

    let (first_tok, end_pos) = match range {
        None => (root_node.first_token(), u32::MAX.into()),
        Some(range) => (
            root_node.token_at_offset(range.start()).right_biased(),
            range.end(),
        ),
    };

    std::iter::successors(first_tok, |tok| tok.next_token())
        .take_while(|tok| tok.text_range().start() < end_pos)
        .filter_map(|tok| {
            Some(HlRange {
                range: tok.text_range(),
                tag: token_tag(&tok)?,
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
        let FilePos { file_id, pos } = f[0];
        assert_eq!(db.parse(file_id).errors(), &[]);
        let hls = super::highlight(&db, file_id, None);
        // No overlapping.
        for w in hls.windows(2) {
            assert!(w[0].range.end() <= w[1].range.start());
        }
        let hlrange = hls
            .iter()
            .find(|hlrange| hlrange.range.contains(pos))
            .expect("No highlight found");
        let got = format!("{:?}", hlrange.tag);
        expect.assert_eq(&got);
    }

    #[test]
    fn keyword() {
        check("$0if 1 then 2 else 3", expect!["Keyword(Conditional)"]);
        check("$0let in 1", expect!["Keyword(Other)"]);
        check("a.b $0or c", expect!["Keyword(Operator)"]);

        // Contextual keywords.
        check("let or = 1; in a $0or", expect!["NameRef(LetIn)"]);
        check("{ $0or = 1; }", expect!["NameDef(PlainAttrset)"]);
    }

    #[test]
    fn operator() {
        check("1 $0+ 1", expect!["Operator(Arithmetic)"]);
        check("1 $0< 1", expect!["Operator(Comparison)"]);
        check("true $0-> false", expect!["Operator(Logical)"]);
        check("[] $0++ []", expect!["Operator(Aggregation)"]);
    }

    #[test]
    fn comment() {
        check("1/*$0a*/", expect!["Comment"]);
        check("1#$0a", expect!["Comment"]);
    }

    #[test]
    fn path() {
        check("$0./.", expect!["Path"]);
    }

    #[test]
    fn literal() {
        check("$042", expect!["IntLiteral"]);
        check("$01.0", expect!["FloatLiteral"]);
        check("$0a:b", expect!["StringLiteral"]);
        check(r#"$0"string""#, expect!["StringLiteral"]);
        check(r#""st$0\nring""#, expect!["StringEscape"]);
    }

    #[test]
    fn builtins_global() {
        check("$0true", expect!["BoolLiteral"]);
        check("$0null", expect!["Builtin(Const)"]);
        check("$0builtins", expect!["Builtin(Attrset)"]);
        check("$0map", expect!["Builtin(Function)"]);
    }

    #[test]
    fn builtins_attrpath() {
        check("builtins.$0head", expect!["Builtin(Function)"]);
        check("builtins.$0not_exist", expect!["AttrField(Select)"]);
    }

    #[test]
    fn builtins_special() {
        check("with builtins; $0head", expect!["Builtin(Function)"]);
        check("with builtins; $0not_exist", expect!["AttrField(With)"]);

        check(
            "let inherit (builtins) head; in $0head",
            expect!["Builtin(Function)"],
        );
        check(
            "let inherit (builtins) head; in $0tail",
            expect!["UnresolvedRef"],
        );
    }

    #[test]
    fn name() {
        check("let $0a = 1; in a", expect!["NameDef(LetIn)"]);
        check("let a = 1; in $0a", expect!["NameRef(LetIn)"]);

        check("$0a: a", expect!["NameDef(Param)"]);
        check("a: $0a", expect!["NameRef(Param)"]);
        check("{ $0a }@b: a", expect!["NameDef(PatField)"]);
        check("{ a }@b: $0a", expect!["NameRef(PatField)"]);
        check("{ a }@$0b: b", expect!["NameDef(Param)"]);
        check("{ a }@b: $0b", expect!["NameRef(Param)"]);

        check("rec { $0a = 1; b = a; }", expect!["NameDef(RecAttrset)"]);
        check("rec { a = 1; b = $0a; }", expect!["NameRef(RecAttrset)"]);

        check(
            "let a = 1; in { inherit $0a; }",
            expect!["NameDef(PlainAttrset)"],
        );
        check(
            "rec { a = 1; b = { inherit $0a; }; }",
            expect!["NameDef(PlainAttrset)"],
        );

        check("let true = 1; in $0true", expect!["NameRef(LetIn)"]);

        check("with {}; $0a", expect!["AttrField(With)"]);

        check("$0not_found", expect!["UnresolvedRef"]);
    }

    #[test]
    fn attr() {
        check("{}.$0a", expect!["AttrField(Select)"]);
        check("{} ? $0a", expect!["AttrField(Select)"]);
    }
}
