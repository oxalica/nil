use crate::def::{AstPtr, NameKind};
use crate::{DefDatabase, FileId, FilePos};
use builtin::{BuiltinKind, ALL_BUILTINS};
use either::Either::{Left, Right};
use rowan::ast::AstNode;
use smol_str::SmolStr;
use syntax::{ast, best_token_at_offset, match_ast, SyntaxKind, TextRange, T};

#[rustfmt::skip]
const EXPR_POS_KEYWORDS: &[&str] = &[
    "assert",
    // "else",
    "if",
    // "in",
    // "inherit",
    "let",
    "or",
    "rec",
    // "then",
    "with",
];
const ATTR_POS_KEYWORDS: &[&str] = &["inherit"];

/// A single completion variant in the editor pop-up.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionItem {
    /// The label to show in the completion menu.
    pub label: SmolStr,
    /// Range of identifier that is being completed.
    pub source_range: TextRange,
    /// What content replaces the source range when user selects this item.
    pub replace: SmolStr,
    /// What item (struct, function, etc) are we completing.
    pub kind: CompletionItemKind,
    /// A brief summary.
    pub brief: Option<String>,
    /// The detailed documentation.
    pub doc: Option<String>,
}

/// The type of the completion item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CompletionItemKind {
    Keyword,
    Param,
    LetBinding,
    Field,
    BuiltinConst,
    BuiltinFunction,
    BuiltinAttrset,
}

impl From<BuiltinKind> for CompletionItemKind {
    fn from(k: BuiltinKind) -> Self {
        match k {
            BuiltinKind::Const => Self::BuiltinConst,
            BuiltinKind::Function => Self::BuiltinFunction,
            BuiltinKind::Attrset => Self::BuiltinAttrset,
        }
    }
}

impl TryFrom<NameKind> for CompletionItemKind {
    type Error = ();
    fn try_from(k: NameKind) -> Result<Self, Self::Error> {
        match k {
            NameKind::LetIn => Ok(Self::LetBinding),
            NameKind::RecAttrset => Ok(Self::Field),
            NameKind::Param | NameKind::PatField => Ok(Self::Param),
            NameKind::PlainAttrset => Err(()),
        }
    }
}

pub(crate) fn completions(
    db: &dyn DefDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<Vec<CompletionItem>> {
    let parse = db.parse(file_id);

    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;
    let source_range = match tok.kind() {
        T![.] => TextRange::empty(pos),
        SyntaxKind::IDENT => tok.text_range(),
        _ => return None,
    };

    let node = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => Some(Left(n)),
                ast::Name(n) => Some(Right(n)),
                _ => None,
            }
        }
    })?;

    match node {
        Left(ref_node) => complete_expr(db, file_id, source_range, ref_node),
        Right(name_node) => {
            let path_node = ast::Attrpath::cast(name_node.syntax().parent()?)?;
            let _entry_node = ast::AttrpathValue::cast(path_node.syntax().parent()?)?;
            complete_attrpath_def(db, file_id, source_range, path_node, name_node)
        }
    }
}

fn complete_expr(
    db: &dyn DefDatabase,
    file_id: FileId,
    source_range: TextRange,
    ref_node: ast::Ref,
) -> Option<Vec<CompletionItem>> {
    let module = db.module(file_id);
    let source_map = db.source_map(file_id);
    let expr_id = source_map.expr_for_node(AstPtr::new(ref_node.syntax()))?;
    let scopes = db.scopes(file_id);
    let scope_id = scopes.scope_for_expr(expr_id)?;

    let prefix = SmolStr::from(ref_node.token()?.text());
    let mut items = Vec::new();
    let mut feed = |compe: CompletionItem| {
        if can_complete(&prefix, &compe.replace) {
            items.push(compe);
        }
    };

    // Keywords.
    EXPR_POS_KEYWORDS
        .iter()
        .map(|kw| keyword_to_completion(kw, source_range))
        .for_each(&mut feed);

    // Contectual keywords.
    if ref_node
        .syntax()
        .ancestors()
        .find_map(ast::IfThenElse::cast)
        .is_some()
    {
        feed(keyword_to_completion("then", source_range));
        feed(keyword_to_completion("else", source_range));
    }
    if ref_node
        .syntax()
        .ancestors()
        .find_map(ast::LetIn::cast)
        .is_some()
    {
        feed(keyword_to_completion("in", source_range));
    }

    // Names in current scopes.
    scopes
        .ancestors(scope_id)
        .filter_map(|scope| scope.as_definitions())
        .flatten()
        .map(|(text, name)| CompletionItem {
            label: text.clone(),
            source_range,
            replace: text.clone(),
            kind: module[*name]
                .kind
                .try_into()
                .expect("NonRecAttrset names are not definitions"),
            brief: None,
            doc: None,
        })
        .for_each(&mut feed);

    // Global builtins.
    ALL_BUILTINS
        .entries()
        .filter(|(_, b)| b.is_global)
        .map(|(name, b)| CompletionItem {
            label: name.into(),
            source_range,
            replace: name.into(),
            kind: b.kind.into(),
            brief: Some(b.summary.into()),
            doc: b.doc.map(|s| s.to_owned()),
        })
        .for_each(&mut feed);

    // TODO: Better sorting.
    items.sort_by(|lhs, rhs| lhs.label.cmp(&rhs.label));
    items.dedup_by(|lhs, rhs| lhs.label == rhs.label);

    Some(items)
}

fn complete_attrpath_def(
    _db: &dyn DefDatabase,
    _file_id: FileId,
    source_range: TextRange,
    path_node: ast::Attrpath,
    _name_node: ast::Name,
) -> Option<Vec<CompletionItem>> {
    if path_node.attrs().count() >= 2 {
        return None;
    }
    let in_let = path_node
        .syntax()
        .ancestors()
        .find_map(ast::LetIn::cast)
        .is_some();
    Some(
        ATTR_POS_KEYWORDS
            .iter()
            .copied()
            .chain(in_let.then_some("in"))
            .map(|kw| keyword_to_completion(kw, source_range))
            .collect(),
    )
}

fn keyword_to_completion(kw: &str, source_range: TextRange) -> CompletionItem {
    CompletionItem {
        label: kw.into(),
        source_range,
        replace: kw.into(),
        kind: CompletionItemKind::Keyword,
        brief: None,
        doc: None,
    }
}

// Subsequence matching.
fn can_complete(prefix: &str, replace: &str) -> bool {
    let mut rest = prefix.as_bytes();
    if rest.is_empty() {
        return true;
    }
    for b in replace.bytes() {
        if rest.first().unwrap() == &b {
            rest = &rest[1..];
            if rest.is_empty() {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check_no(fixture: &str, label: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        if let Some(compes) = super::completions(&db, f[0]) {
            assert_eq!(compes.iter().find(|item| item.label == label), None);
        }
    }

    #[track_caller]
    fn check(fixture: &str, label: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let compes = super::completions(&db, f[0]).expect("No completion");
        let item = compes
            .iter()
            .find(|item| item.label == label)
            .expect("No expected completion");

        let source_range =
            usize::from(item.source_range.start())..usize::from(item.source_range.end());
        let mut completed = db.file_content(f[0].file_id).to_string();
        completed.replace_range(source_range, &item.replace);
        let got = format!("({:?}) {}", item.kind, completed);
        expect.assert_eq(&got);
    }

    #[test]
    fn keyword() {
        check("l$0", "let", expect!["(Keyword) let"]);
        check("i$0", "if", expect!["(Keyword) if"]);

        // Cannot complete.
        check_no("tl$0", "let");
        // Not in context.
        check_no("i$0", "in");
        check_no("th$0", "then");

        check("let i$0", "in", expect!["(Keyword) let in"]);
        check("if a th$0", "then", expect!["(Keyword) if a then"]);
    }

    #[test]
    fn local_binding() {
        check(
            "foo: ({ bar ? b$0 }: 0) b",
            "bar",
            expect!["(Param) foo: ({ bar ? bar }: 0) b"],
        );
        check_no("(foo: ({ bar ? b }: 0) b$0", "bar");

        check(
            "let foo = b$0; bar = 2;",
            "bar",
            expect!["(LetBinding) let foo = bar; bar = 2;"],
        );
        check(
            "rec { foo = b$0; bar = 2; }",
            "bar",
            expect!["(Field) rec { foo = bar; bar = 2; }"],
        );
    }

    #[test]
    fn builtin() {
        check("toS$0", "toString", expect!["(BuiltinFunction) toString"]);
        check("t$0", "true", expect!["(BuiltinConst) true"]);
        check("b$0", "builtins", expect!["(BuiltinAttrset) builtins"]);

        // No prim-ops.
        check_no("__al$0", "__all");
        // No non-global builtins.
        check_no("attrN$0", "attrNames");
    }

    #[test]
    fn inherit() {
        check("{ i$0 }", "inherit", expect!["(Keyword) { inherit }"]);
        check("let i$0", "inherit", expect!["(Keyword) let inherit"]);
        check_no("let a = i$0", "inherit");
        check_no("let a.i$0", "inherit");
        check_no("let a.${i$0", "inherit");
    }
}
