use crate::def::{AstPtr, BindingValue, Expr, NameKind};
use crate::{FileId, FilePos, TyDatabase};
use builtin::{BuiltinKind, ALL_BUILTINS};
use either::Either::{Left, Right};
use rowan::ast::AstNode;
use smol_str::SmolStr;
use syntax::ast::Attr;
use syntax::semantic::AttrKind;
use syntax::{ast, best_token_at_offset, match_ast, SyntaxKind, SyntaxNode, TextRange, T};

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
    db: &dyn TyDatabase,
    fpos @ FilePos { file_id, pos }: FilePos,
    trigger_char: Option<char>,
) -> Option<Vec<CompletionItem>> {
    let parse = db.parse(file_id);

    if let Some(items) =
        trigger_char.and_then(|ch| complete_trigger(db, fpos, parse.syntax_node(), ch))
    {
        return Some(items);
    }

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
        Right(name_node) => complete_attrpath(db, file_id, source_range, name_node),
    }
}

fn complete_trigger(
    db: &dyn TyDatabase,
    FilePos { file_id, pos }: FilePos,
    root_node: SyntaxNode,
    trigger_char: char,
) -> Option<Vec<CompletionItem>> {
    if !matches!(trigger_char, '.' | '?') {
        return None;
    }

    let trigger_tok = root_node.token_at_offset(pos).left_biased()?;
    let source_range = TextRange::empty(trigger_tok.text_range().end());
    let path_node = match_ast! {
        match (trigger_tok.parent()?) {
            // `foo.bar.|` or `foo?bar.|`
            ast::Attrpath(n) => n,
            // `foo.|`
            ast::Select(n) => n.attrpath()?,
            // `foo?|`
            ast::HasAttr(n) => n.attrpath()?,
            _ => return None,
        }
    };

    // There may be triva between `.` and NAME, due to parser lookahead.
    // We skips them to locate the correct node, while keeping `source_range` at the cursor.
    // `{ a.| = 42; }`
    //     ^  \ NAME before here
    //     DOT is here
    let name_node = match path_node
        .attrs()
        .find(|attr| source_range.start() <= attr.syntax().text_range().start())?
    {
        Attr::Name(name) => name,
        _ => return None,
    };

    complete_attrpath(db, file_id, source_range, name_node)
}

fn complete_expr(
    db: &dyn TyDatabase,
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

fn complete_attrpath(
    db: &dyn TyDatabase,
    file_id: FileId,
    source_range: TextRange,
    name_node: ast::Name,
) -> Option<Vec<CompletionItem>> {
    let path_node = ast::Attrpath::cast(name_node.syntax().parent()?)?;
    let (set_node, container_node) = match_ast! {
        match (path_node.syntax().parent()?){
            ast::AttrpathValue(n) => {
                let n = n.syntax().parent()?;
                (n.clone(), n)
            },
            ast::HasAttr(n) => (n.set()?.syntax().clone(), n.syntax().clone()),
            ast::Select(n) => (n.set()?.syntax().clone(), n.syntax().clone()),
            _ => return None,
        }
    };

    let is_let = ast::LetIn::can_cast(container_node.kind());
    let is_attrset = ast::AttrSet::can_cast(container_node.kind());
    let attr_cnt = path_node.attrs().count();
    let current_input = name_node
        .token()
        .map_or(SmolStr::default(), |tok| tok.text().into());

    let module = db.module(file_id);
    let source_map = db.source_map(file_id);

    let mut items = Vec::new();

    // Only complete keywords when this Attrpath has only one Attr.
    if attr_cnt == 1 && (is_let || is_attrset) {
        items.push(keyword_to_completion("inherit", source_range));
    }
    if attr_cnt == 1 && is_let {
        items.push(keyword_to_completion("in", source_range));
    }

    // We are inside the first Attr of a Let.
    // Completes all static names in the same `Let` for splited definition.
    // ```nix
    // let
    //   foo.bar = 42;
    //   f| # <- We may want to also define `foo.baz`.
    // in /**/
    // ```
    // Note that the first Attr of an Attrset can be handled by types later.
    if is_let
        && path_node
            .attrs()
            .next()
            .map_or(false, |attr| attr.syntax() == name_node.syntax())
    {
        if let Some(expr) = source_map.expr_for_node(AstPtr::new(&container_node)) {
            if let Expr::LetIn(b, _) = &module[expr] {
                items.extend(
                    b.statics
                        .iter()
                        .filter(|(_, v)| matches!(v, BindingValue::Expr(_)))
                        .map(|&(name, _)| module[name].text.clone())
                        // We should not report current incomplete definition.
                        // This is covered by `no_incomplete_field`.
                        .filter(|name| *name != current_input)
                        .map(|name| CompletionItem {
                            label: name.clone(),
                            source_range,
                            replace: name,
                            kind: CompletionItemKind::LetBinding,
                            brief: None,
                            doc: None,
                        }),
                );
            }
        }
        return Some(items);
    }

    // If we get here, we are either inside a selection path `a.b|`,
    // or non-first parts of a definition `{ a.b| }`.
    // Use type information
    (|| -> Option<()> {
        let infer = db.infer(file_id);

        let mut attrs = path_node.attrs();
        let set_ty = if is_let {
            let name = source_map.name_for_node(AstPtr::new(attrs.next()?.syntax()))?;
            infer.ty_for_name(name)
        } else {
            let set_expr = source_map.expr_for_node(AstPtr::new(&set_node))?;
            infer.ty_for_expr(set_expr)
        };

        // Resolve prefix paths, except for the current NAME.
        // foo.a.b.c|.d
        // ^-----^
        let set = attrs
            .take_while(|attr| attr.syntax() != name_node.syntax())
            .try_fold(set_ty, |set_ty, attr| match AttrKind::of(attr) {
                AttrKind::Static(Some(field)) => set_ty.kind(&infer).as_attrset()?.get(&field),
                _ => None,
            })?
            .kind(&infer)
            .as_attrset()?;

        items.extend(
            set.iter()
                // We should not report current incomplete definition.
                // This is covered by `no_incomplete_field`.
                .filter(|(name, _)| **name != current_input)
                .map(|(name, ty)| CompletionItem {
                    label: name.clone(),
                    source_range,
                    replace: name.clone(),
                    kind: CompletionItemKind::Field,
                    brief: Some(infer.display_ty(ty).to_string()),
                    doc: None,
                }),
        );

        Some(())
    })();

    Some(items)
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
        if let Some(compes) = super::completions(&db, f[0], None) {
            assert_eq!(compes.iter().find(|item| item.label == label), None);
        }
    }

    #[track_caller]
    fn check_trigger(fixture: &str, trigger_char: Option<char>, label: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let compes = super::completions(&db, f[0], trigger_char).expect("No completion");
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

    #[track_caller]
    fn check(fixture: &str, label: &str, expect: Expect) {
        check_trigger(fixture, None, label, expect);
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

    #[test]
    fn select_known_field() {
        check(
            "{ foo.bar = 1; }.f$0",
            "foo",
            expect!["(Field) { foo.bar = 1; }.foo"],
        );
        check(
            "{ foo.bar = 1; }.f$0.bar",
            "foo",
            expect!["(Field) { foo.bar = 1; }.foo.bar"],
        );
        check(
            "{ foo.bar = 1; }.foo.b$0",
            "bar",
            expect!["(Field) { foo.bar = 1; }.foo.bar"],
        );

        check(
            "let a.foo = 1; in a.f$0",
            "foo",
            expect!["(Field) let a.foo = 1; in a.foo"],
        );
        check(
            "{ foo }@b: b.f$0",
            "foo",
            expect!["(Field) { foo }@b: b.foo"],
        );
    }

    #[test]
    fn trigger_select_known_field() {
        check_trigger(
            "{ foo.bar = 1; }.$0",
            Some('.'),
            "foo",
            expect!["(Field) { foo.bar = 1; }.foo"],
        );
        check_trigger(
            "{ foo.bar = 1; }.$0.bar",
            Some('.'),
            "foo",
            expect!["(Field) { foo.bar = 1; }.foo.bar"],
        );
        check_trigger(
            "{ foo.bar = 1; }.foo.$0",
            Some('.'),
            "bar",
            expect!["(Field) { foo.bar = 1; }.foo.bar"],
        );
    }

    #[test]
    fn has_known_field() {
        check(
            "{ foo.bar = 1; } ? f$0",
            "foo",
            expect!["(Field) { foo.bar = 1; } ? foo"],
        );
        check(
            "{ foo.bar = 1; } ? f$0.bar",
            "foo",
            expect!["(Field) { foo.bar = 1; } ? foo.bar"],
        );
        check(
            "{ foo.bar = 1; } ? foo.b$0",
            "bar",
            expect!["(Field) { foo.bar = 1; } ? foo.bar"],
        );
    }

    #[test]
    fn trigger_has_known_field() {
        check_trigger(
            "{ foo.bar = 1; }?$0",
            Some('?'),
            "foo",
            expect!["(Field) { foo.bar = 1; }?foo"],
        );
        check_trigger(
            "{ foo.bar = 1; }?foo.$0",
            Some('.'),
            "bar",
            expect!["(Field) { foo.bar = 1; }?foo.bar"],
        );
        check_trigger(
            "{ foo.bar = 1; }?$0.bar",
            Some('.'),
            "foo",
            expect!["(Field) { foo.bar = 1; }?foo.bar"],
        );
    }

    #[test]
    fn define_known_field_let() {
        check(
            "let a.f$0 = 1; in a.foo.bar",
            "foo",
            expect!["(Field) let a.foo = 1; in a.foo.bar"],
        );
        check(
            "let a.f$0.bar = 1; in a.foo.bar",
            "foo",
            expect!["(Field) let a.foo.bar = 1; in a.foo.bar"],
        );
        check(
            "let a.foo.b$0 = 1; in a.foo.bar",
            "bar",
            expect!["(Field) let a.foo.bar = 1; in a.foo.bar"],
        );
    }

    #[test]
    fn trigger_define_known_field_let() {
        check_trigger(
            "let a.$0 = 1; in a.foo.bar",
            Some('.'),
            "foo",
            expect!["(Field) let a.foo = 1; in a.foo.bar"],
        );
        check_trigger(
            "let a.$0.bar = 1; in a.foo.bar",
            Some('.'),
            "foo",
            expect!["(Field) let a.foo.bar = 1; in a.foo.bar"],
        );
        check_trigger(
            "let a.foo.$0 = 1; in a.foo.bar",
            Some('.'),
            "bar",
            expect!["(Field) let a.foo.bar = 1; in a.foo.bar"],
        );
    }

    #[test]
    fn define_known_field_attrset() {
        check(
            "let f = { foo }: foo.bar; in f { f$0 }",
            "foo",
            expect!["(Field) let f = { foo }: foo.bar; in f { foo }"],
        );
        check(
            "let f = { foo }: foo.bar; in f { f$0.bar }",
            "foo",
            expect!["(Field) let f = { foo }: foo.bar; in f { foo.bar }"],
        );
        check(
            "let f = { foo }: foo.bar; in f { foo.b$0 }",
            "bar",
            expect!["(Field) let f = { foo }: foo.bar; in f { foo.bar }"],
        );
    }

    #[test]
    fn define_let_sibling() {
        check(
            "let foo.bar = 1; f$0 in 1",
            "foo",
            expect!["(LetBinding) let foo.bar = 1; foo in 1"],
        );
    }

    #[test]
    fn no_incomplete_field() {
        check_no("a: a.f$0", "f");
        check_no("{ f$0 = 1; }", "f");
        check_no("let f$0 = 1; in 1", "f");
    }
}
