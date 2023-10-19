use crate::def::{AstPtr, BindingValue, Expr, ExprId, ModuleScopes, NameKind};
use crate::ty::{self, AttrSource, DisplayConfig, Ty};
use crate::{FilePos, InferenceResult, Module, ModuleSourceMap, TyDatabase};
use builtin::{BuiltinKind, ALL_BUILTINS};
use smol_str::SmolStr;
use syntax::ast::{self, AstNode};
use syntax::rowan::TokenAtOffset;
use syntax::semantic::{escape_literal_attr, is_valid_ident, AttrKind};
use syntax::{match_ast, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, T};

use super::hover::TY_DETAILED_DISPLAY;

pub const TY_SIGNATURE_DISPLAY: DisplayConfig = DisplayConfig {
    max_lambda_lhs_depth: 2,
    max_list_depth: 4,
    max_attrset_depth: 0,
    max_attrset_fields: 0,
    lambda_need_parentheses: false,
};

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
    pub replace_range: TextRange,
    /// What content replaces the source range when user selects this item.
    pub replace: SmolStr,
    /// What item (struct, function, etc) are we completing.
    pub kind: CompletionItemKind,
    /// Type signature.
    pub signature: Option<String>,
    /// A brief description.
    pub description: Option<String>,
    /// The detailed documentation.
    pub documentation: Option<String>,
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

impl From<NameKind> for CompletionItemKind {
    fn from(k: NameKind) -> Self {
        match k {
            NameKind::LetIn => Self::LetBinding,
            NameKind::RecAttrset => Self::Field,
            NameKind::Param | NameKind::PatField => Self::Param,
            NameKind::PlainAttrset => Self::Field,
        }
    }
}

struct Context<'a> {
    module: &'a Module,
    source_map: &'a ModuleSourceMap,
    scopes: &'a ModuleScopes,
    infer: &'a InferenceResult,
    fpos: FilePos,
    // The token at cursor (left biased) to complete.
    token: SyntaxToken,
    // The replace range for the result.
    replace_range: TextRange,
    // Identifier prefix as filter.
    prefix: &'a str,
    completions: Vec<CompletionItem>,
}

pub(crate) fn completions(
    db: &dyn TyDatabase,
    fpos @ FilePos { file_id, pos }: FilePos,
    _trigger_char: Option<char>,
) -> Vec<CompletionItem> {
    let parse = db.parse(file_id);

    // Always completes from the LHS if we are in the middle of two tokens,
    // with the exception when LHS is the binding terminator `;`,
    // then we must regard it as a new binding starting.
    let token = match parse.syntax_node().token_at_offset(pos) {
        TokenAtOffset::None => return Vec::new(),
        TokenAtOffset::Single(token) => token,
        TokenAtOffset::Between(lhs, _) if lhs.kind() != T![;] => lhs,
        TokenAtOffset::Between(_, rhs) => rhs,
    };

    // Identifiers on LHS (keywords may be incomplete identifiers) are the hints to complete,
    // the result should replace it.
    // Otherwise, if we are not in (or after) any identifiers, we are completing a fresh new
    // identifier or snippet, the result should be inserted to the current position.
    let (replace_range, prefix) = if token.kind() == SyntaxKind::IDENT || token.kind().is_keyword()
    {
        // Clamp the prefix range in case where we are typing inside an identifier.
        // Eg. `foo|bar`. This is not clear if we want to replace `foobar` or just insert,
        // we do the former currently. But at least the hint prefix must be only `foo`.
        let prefix_len = pos - token.text_range().start();
        (token.text_range(), &token.text()[..prefix_len.into()])
    } else {
        (TextRange::empty(pos), "")
    };

    let module = db.module(file_id);
    let source_map = db.source_map(file_id);
    let scopes = db.scopes(file_id);
    let infer = db.infer(file_id);

    let mut ctx = Context {
        module: &module,
        source_map: &source_map,
        scopes: &scopes,
        infer: &infer,
        fpos,
        token: token.clone(),
        replace_range,
        prefix,
        completions: Vec::new(),
    };
    ctx.complete();

    let mut completions = ctx.completions;
    // TODO: Better sorting.
    completions.sort_by(|lhs, rhs| lhs.label.cmp(&rhs.label));
    completions.dedup_by(|lhs, rhs| lhs.label == rhs.label);
    completions
}

impl Context<'_> {
    fn complete(&mut self) -> Option<()> {
        // Do not complete inside strings.
        // TODO: Escapes and `${}` snippets?
        if let T!["''"] | T!['"'] | SyntaxKind::STRING_FRAGMENT = self.token.kind() {
            return None;
        }

        let mut parent = self.token.parent()?;
        if let Some(name) = ast::Name::cast(parent.clone()) {
            parent = name.syntax().parent()?;
        }

        match_ast! {
            match parent {
                // A definition, or a child of `HasAttr` or `Select`.
                ast::Attrpath(path) => {
                    self.complete_attrpath(path);
                },
                ast::Inherit(inherit_node) => {
                    self.complete_inherit_attr(inherit_node);
                },
                // Triggered right at `.` or `?` token.
                ast::HasAttr(e) => {
                    if e.question_token()?.text_range().end() <= self.fpos.pos {
                        self.complete_attrpath(e.attrpath()?);
                    }
                },
                ast::Select(e) => {
                    if e.dot_token()?.text_range().end() <= self.fpos.pos {
                        self.complete_attrpath(e.attrpath()?);
                    }
                },

                // Outside any existing binding but inside a binding container,
                // eg. `let |` or `{ a = 1; | }`.
                ast::LetIn(e) => {
                    self.complete_binding(ast::Expr::LetIn(e));
                },
                ast::AttrSet(e) => {
                    self.complete_binding(ast::Expr::AttrSet(e));
                },

                // At a parameter pattern field,
                // eg. `{ a| }: 42`.
                ast::PatField(field) => {
                    let lambda = field
                        .syntax()
                        .ancestors()
                        // Lambda > Param > Pat > PatField
                        .nth(3)
                        .and_then(ast::Lambda::cast)?;
                    self.complete_lambda_pat_param(lambda);
                },
                // Inside lambda parameter but outside any existing fields,
                // eg. `{ a, |... }: 42`
                ast::Pat(pat) => {
                    let lambda = pat
                        .syntax()
                        .ancestors()
                        // Lambda > Param > Pat
                        .nth(2)
                        .and_then(ast::Lambda::cast)?;
                    self.complete_lambda_pat_param(lambda);
                },

                // Expression context.
                ast::Expr(expr) => {
                    self.complete_expr(expr);
                },
                _ => {}
            }
        }

        Some(())
    }

    /// Subsequence matching check.
    fn can_complete(&self, replace: &str) -> bool {
        let mut prefix = self.prefix.as_bytes();
        if prefix.is_empty() {
            return true;
        }
        for b in replace.bytes() {
            if prefix.first().unwrap() == &b {
                prefix = &prefix[1..];
                if prefix.is_empty() {
                    return true;
                }
            }
        }
        false
    }

    fn record_item(&mut self, compe: CompletionItem) {
        if self.can_complete(&compe.replace) {
            self.completions.push(compe);
        }
    }

    fn record_keyword(&mut self, kw: &str) {
        self.record_item(CompletionItem {
            label: kw.into(),
            replace_range: self.replace_range,
            replace: kw.into(),
            kind: CompletionItemKind::Keyword,
            signature: None,
            description: None,
            documentation: None,
        });
    }

    fn record_builtin(&mut self, name: &str) {
        let Some(builtin) = ALL_BUILTINS.get(name) else {
            return;
        };
        let ty = ty::known::BUILTINS
            .as_attrset()
            .unwrap()
            .get(name)
            .cloned()
            .unwrap_or(Ty::Unknown);
        self.record_item(CompletionItem {
            label: name.into(),
            replace_range: self.replace_range,
            replace: name.into(),
            kind: builtin.kind.into(),
            signature: ty
                .is_known()
                .then(|| ty.display_with(TY_SIGNATURE_DISPLAY).to_string()),
            description: Some(format!(
                "{}\n{}",
                ty.display_with(TY_DETAILED_DISPLAY),
                builtin.summary,
            )),
            documentation: builtin.doc.map(|s| s.to_owned()),
        });
    }

    /// Complete in expression position, with the name scope of `expr_node`.
    /// Eg. `a + |` or `let a = 1; in |`.
    fn complete_expr(&mut self, expr_node: ast::Expr) -> Option<()> {
        let expr_id = self
            .source_map
            .expr_for_node(AstPtr::new(expr_node.syntax()))?;

        // Keywords.
        for kw in EXPR_POS_KEYWORDS {
            self.record_keyword(kw);
        }

        // Contextual keywords.
        // TODO: Only emit if they are actually missing.
        if expr_node
            .syntax()
            .ancestors()
            .find_map(ast::IfThenElse::cast)
            .is_some()
        {
            self.record_keyword("then");
            self.record_keyword("else");
        }
        if expr_node
            .syntax()
            .ancestors()
            .find_map(ast::LetIn::cast)
            .is_some()
        {
            self.record_keyword("in");
        }

        // Global builtins.
        ALL_BUILTINS
            .entries()
            .filter(|(_, b)| b.is_global)
            .for_each(|(name, _)| self.record_builtin(name));

        let scope_id = self.scopes.scope_for_expr(expr_id)?;

        // Names in current scopes.
        self.scopes
            .ancestors(scope_id)
            .filter_map(|scope| scope.as_definitions())
            .flatten()
            .filter(|(text, _)| is_valid_ident(text))
            .for_each(|(text, &name)| {
                self.record_item(CompletionItem {
                    label: text.clone(),
                    replace_range: self.replace_range,
                    replace: text.clone(),
                    kind: self.module[name].kind.into(),
                    signature: {
                        let ty = self.infer.ty_for_name(name);
                        ty.is_known()
                            .then(|| ty.display_with(TY_SIGNATURE_DISPLAY).to_string())
                    },
                    description: None,
                    documentation: None,
                });
            });

        Some(())
    }

    /// Complete in binding position.
    /// Eg. `{ a = 1; | }` or `let |`.
    fn complete_binding(&mut self, container: ast::Expr) -> Option<()> {
        self.record_keyword("inherit");

        let expr_id = self
            .source_map
            .expr_for_node(AstPtr::new(container.syntax()))?;
        if let ast::Expr::LetIn(let_in_node) = &container {
            if let_in_node.in_token().is_none() {
                self.record_keyword("in");
            }
            self.complete_sibling_bindings(expr_id);
        } else if let ast::Expr::AttrSet(attrset_node) = &container {
            let is_rec = attrset_node.rec_token().is_some();
            let is_let = attrset_node.let_token().is_some();
            if is_rec || is_let {
                self.complete_sibling_bindings(expr_id);
            }
            if !is_let {
                let ty = self.infer.ty_for_expr(expr_id);
                self.complete_attr(ty);
            }
        }

        Some(())
    }

    // Complete variables defined in the same block for recursive blocks.
    // `let foo.bar = 1; f|` -> `foo`
    // TODO: Filter variables with Attrset types?
    fn complete_sibling_bindings(&mut self, expr_id: ExprId) -> Option<()> {
        let (Expr::LetIn(bindings, _) | Expr::RecAttrset(bindings) | Expr::LetAttrset(bindings)) =
            &self.module[expr_id]
        else {
            return None;
        };

        let prefix = self.prefix;
        bindings
            .statics
            .iter()
            .filter(|(_, v)| matches!(v, BindingValue::Expr(_)))
            .map(|&(name, _)| self.module[name].text.clone())
            // Skip current incomplete prefix.
            // This is covered by `no_incomplete_field`.
            .filter(|name| *name != prefix)
            .for_each(|name| {
                let escaped_name = escape_literal_attr(&name);
                self.record_item(CompletionItem {
                    label: escaped_name.as_ref().into(),
                    replace_range: self.replace_range,
                    replace: escaped_name.into(),
                    kind: CompletionItemKind::LetBinding,
                    signature: None,
                    description: None,
                    documentation: None,
                });
            });
        Some(())
    }

    /// Complete attributes of a given attrset type.
    fn complete_attr(&mut self, attrset_ty: Ty) -> Option<()> {
        let attrset = attrset_ty.as_attrset()?;
        for (name, ty, src) in attrset.iter() {
            // Fast filter. And skip current incomplete prefix.
            if !self.can_complete(name) || name == self.prefix {
                continue;
            }

            if src == AttrSource::Builtin {
                self.record_builtin(name);
                continue;
            }

            let escaped_name = escape_literal_attr(name);
            self.completions.push(CompletionItem {
                label: escaped_name.as_ref().into(),
                replace_range: self.replace_range,
                replace: escaped_name.into(),
                kind: match src {
                    AttrSource::Unknown => CompletionItemKind::Field,
                    AttrSource::Name(name) => self.module[name].kind.into(),
                    // Handled above.
                    AttrSource::Builtin => unreachable!(),
                },
                signature: Some(ty.display_with(TY_SIGNATURE_DISPLAY).to_string()),
                description: Some(ty.display_with(TY_DETAILED_DISPLAY).to_string()),
                documentation: None,
            });
        }
        Some(())
    }

    /// Complete a segment of `Attrpath`, which may be a reference or a definition.
    fn complete_attrpath(&mut self, node: ast::Attrpath) -> Option<()> {
        // All known `Attr`s, until (and excluding) the one we are currently typing.
        // foo.a.b.c|.d
        // ^-----^
        let mut prefix_attrs = node
            .attrs()
            .take_while(|attr| attr.syntax().text_range().end() < self.fpos.pos)
            .peekable();

        // TODO: Merge these logic.
        // Currently, we must special case the first `Attr` of let-in definition to get its type,
        // since the `Expr::LetIn` has the type of its body, not the variable set.
        enum Prefix {
            SetExpr(SyntaxNode),
            LetIn(ast::Attr),
        }

        let prefix = match_ast! {
            match (node.syntax().parent()?){
                ast::HasAttr(n) => Prefix::SetExpr(n.set()?.syntax().clone()),
                ast::Select(n) => Prefix::SetExpr(n.set()?.syntax().clone()),
                ast::AttrpathValue(n) => {
                    // We are typing the first word of a binding.
                    if prefix_attrs.peek().is_none() {
                        return self.complete_binding(ast::Expr::cast(n.syntax().parent()?)?);
                    }

                    match_ast! {
                        match (n.syntax().parent()?) {
                            ast::AttrSet(n) => {
                                Prefix::SetExpr(n.syntax().clone())
                            },
                            ast::LetIn(_) => {
                                Prefix::LetIn(prefix_attrs.next().expect("handled above"))
                            },
                            _ => return None,
                        }
                    }
                },
                _ => return None,
            }
        };

        let set_ty = match prefix {
            Prefix::SetExpr(n) => {
                let expr = self.source_map.expr_for_node(AstPtr::new(&n))?;
                self.infer.ty_for_expr(expr)
            }
            Prefix::LetIn(first_attr) => {
                let name = self
                    .source_map
                    .name_for_node(AstPtr::new(first_attr.syntax()))?;
                self.infer.ty_for_name(name)
            }
        };

        let set_ty = prefix_attrs.try_fold(set_ty, |set_ty, attr| match AttrKind::of(attr) {
            AttrKind::Static(Some(field)) => set_ty.as_attrset()?.get(&field).cloned(),
            _ => None,
        })?;
        self.complete_attr(set_ty)
    }

    /// Complete an `Attr` of an `inherit` binding.
    fn complete_inherit_attr(&mut self, inherit: ast::Inherit) -> Option<()> {
        let ptr = AstPtr::new(&inherit.syntax().parent()?);
        let container_expr = self.source_map.expr_for_node(ptr)?;
        let scope_id = self.scopes.scope_for_expr(container_expr)?;
        self.scopes
            .ancestors(scope_id)
            .filter_map(|scope| scope.as_definitions())
            .flatten()
            .for_each(|(text, &name)| {
                let escaped_name = escape_literal_attr(text);
                self.record_item(CompletionItem {
                    label: escaped_name.as_ref().into(),
                    replace_range: self.replace_range,
                    replace: escaped_name.into(),
                    kind: self.module[name].kind.into(),
                    signature: {
                        let ty = self.infer.ty_for_name(name);
                        ty.is_known()
                            .then(|| ty.display_with(TY_SIGNATURE_DISPLAY).to_string())
                    },
                    description: None,
                    documentation: None,
                });
            });
        Some(())
    }

    /// Completes a lambda parameter pattern (aka, "formal").
    fn complete_lambda_pat_param(&mut self, lambda: ast::Lambda) -> Option<()> {
        let expr = self
            .source_map
            .expr_for_node(AstPtr::new(lambda.syntax()))?;
        let Ty::Lambda(param_ty, _) = self.infer.ty_for_expr(expr) else {
            return None;
        };
        let prefix = self.prefix;
        param_ty
            .as_attrset()?
            .iter()
            // We should not report current incomplete definition.
            .filter(|(name, ..)| **name != prefix)
            .for_each(|(name, ty, _)| {
                self.record_item(CompletionItem {
                    label: name.clone(),
                    replace_range: self.replace_range,
                    replace: name.clone(),
                    kind: CompletionItemKind::Param,
                    signature: ty
                        .is_known()
                        .then(|| ty.display_with(TY_SIGNATURE_DISPLAY).to_string()),
                    description: Some(ty.display_with(TY_DETAILED_DISPLAY).to_string()),
                    documentation: None,
                });
            });
        Some(())
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;
    use std::sync::Arc;

    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};
    use nix_interop::nixos_options::{self, NixosOption, NixosOptions};

    #[track_caller]
    fn check_no(fixture: &str, label: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let compes = super::completions(&db, f[0], None);
        assert_eq!(compes.iter().find(|item| item.label == label), None);
    }

    #[track_caller]
    fn check_trigger(fixture: &str, trigger_char: Option<char>, label: &str, expect: Expect) {
        let (mut db, f) = TestDB::from_fixture(fixture).expect("fixture should be valid");
        assert_eq!(f.markers().len(), 1, "should have exact one marker");
        db.set_nixos_options(Arc::new(NixosOptions::from_iter([(
            "nix".into(),
            NixosOption {
                ty: nixos_options::Ty::Attrset {
                    fields: NixosOptions::from_iter([(
                        "enable".into(),
                        NixosOption {
                            ty: nixos_options::Ty::Bool,
                            ..NixosOption::default()
                        },
                    )]),
                    rest: None,
                },
                ..NixosOption::default()
            },
        )])));

        let compes = super::completions(&db, f[0], trigger_char);
        let item = compes
            .iter()
            .find(|item| item.label == label)
            .expect("No expected completion");

        let mut completed = db.file_content(f[0].file_id).to_string();
        completed.replace_range(<Range<usize>>::from(item.replace_range), &item.replace);
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
    fn builtin_global() {
        check("toS$0", "toString", expect!["(BuiltinFunction) toString"]);
        check("t$0", "true", expect!["(BuiltinConst) true"]);
        check("b$0", "builtins", expect!["(BuiltinAttrset) builtins"]);

        // No prim-ops.
        check_no("__al$0", "__all");
        // No non-global builtins.
        check_no("attrN$0", "attrNames");
    }

    #[test]
    fn builtin_attrpath() {
        check(
            "builtins.attrNa$0",
            "attrNames",
            expect!["(BuiltinFunction) builtins.attrNames"],
        );
    }

    #[test]
    fn inherit_keyword() {
        check("{ i$0 }", "inherit", expect!["(Keyword) { inherit }"]);
        check("let i$0", "inherit", expect!["(Keyword) let inherit"]);
        check_no("let a = i$0", "inherit");
        check_no("let a.i$0", "inherit");
        check_no("let a.${i$0", "inherit");
    }

    #[test]
    fn inherit_attr() {
        check(
            "let foo = 42; in { inherit $0 }",
            "foo",
            expect!["(LetBinding) let foo = 42; in { inherit foo }"],
        );
        check(
            "let foo = 42; in { inherit f$0; }",
            "foo",
            expect!["(LetBinding) let foo = 42; in { inherit foo; }"],
        );

        check_no("let foo = 42; inherit $0 in 42;", "foo");
        check(
            "foo: let foo = 42; inherit $0 in 42;",
            "foo",
            expect!["(Param) foo: let foo = 42; inherit foo in 42;"],
        );

        check_no("rec { foo = 42; inherit $0; }", "foo");
        check(
            "foo: rec { foo = 42; inherit $0; }",
            "foo",
            expect!["(Param) foo: rec { foo = 42; inherit foo; }"],
        );
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
            expect!["(Param) { foo }@b: b.foo"],
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
            expect!["(Param) let f = { foo }: foo.bar; in f { foo }"],
        );
        check(
            "let f = { foo }: foo.bar; in f { f$0.bar }",
            "foo",
            expect!["(Param) let f = { foo }: foo.bar; in f { foo.bar }"],
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

    #[test]
    fn parameter_definition() {
        check(
            "({ f$0 }: 42) { foo = 42; }",
            "foo",
            expect!["(Param) ({ foo }: 42) { foo = 42; }"],
        );
        check(
            r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee
{
    inputs.nixpkgs.url = "...";
    outputs = { n$0 }: { };
}
"#,
            "nixpkgs",
            expect![[r#"
                (Param) {
                    inputs.nixpkgs.url = "...";
                    outputs = { nixpkgs }: { };
                }"#]],
        );
    }

    #[test]
    fn nixos_config() {
        // Option types are set in `check_trigger`.
        check(
            "
{ ... }:
{
    nix.e$0
}
",
            "enable",
            expect![[r#"
                (Field) { ... }:
                {
                    nix.enable
                }"#]],
        );

        check_no(
            "
{ ... }:
{
    notexists.e$0
}
",
            "enable",
        );
        check_no(
            "
{ stdenv }:
stdenv.mkDerivation {
    nix.e$0
}
",
            "enable",
        );
    }

    #[test]
    fn escape_attr() {
        check(
            r#"{ "foo/bar" = 1; }.f$0"#,
            r#""foo/bar""#,
            expect![[r#"(Field) { "foo/bar" = 1; }."foo/bar""#]],
        );
    }

    #[test]
    fn no_invalid_ident() {
        check_no(r#"let "a b" = 1; in a$0"#, "a b");
        check_no(r#"let "a b" = 1; in a$0"#, r#""a b""#);
    }

    #[test]
    fn fresh_define_let_in() {
        check(
            "let $0 foo.bar = 42; in 42",
            "foo",
            expect!["(LetBinding) let foo foo.bar = 42; in 42"],
        );
        check(
            "let foo.bar = 42; $0 in 42",
            "foo",
            expect!["(LetBinding) let foo.bar = 42; foo in 42"],
        );
    }

    #[test]
    fn fresh_define_attr() {
        check(
            "{ $0 foo.bar = 42; }",
            "foo",
            expect!["(Field) { foo foo.bar = 42; }"],
        );
        check(
            "{ foo.bar = 42; $0 }",
            "foo",
            expect!["(Field) { foo.bar = 42; foo }"],
        );
        check(
            "{ foo.bar = 42;$0 }",
            "foo",
            expect!["(Field) { foo.bar = 42;foo }"],
        );
    }

    #[test]
    fn fresh_define_param_field() {
        check(
            "({ $0 }: 42) { foo = 42; }",
            "foo",
            expect!["(Param) ({ foo }: 42) { foo = 42; }"],
        );
        check(
            "({$0}: 42) { foo = 42; }",
            "foo",
            expect!["(Param) ({foo}: 42) { foo = 42; }"],
        );
        check(
            "({bar,$0}: 42) { foo = 42; }",
            "foo",
            expect!["(Param) ({bar,foo}: 42) { foo = 42; }"],
        );
        check(
            "({ bar, $0 }: 42) { foo = 42; }",
            "foo",
            expect!["(Param) ({ bar, foo }: 42) { foo = 42; }"],
        );
        check(
            "({ $0... }: 42) { foo = 42; }",
            "foo",
            expect!["(Param) ({ foo... }: 42) { foo = 42; }"],
        );
    }

    #[test]
    fn fresh_reference() {
        check(
            "let foo = 42; in $0",
            "foo",
            expect!["(LetBinding) let foo = 42; in foo"],
        );
        check(
            "let foo = 42; in 1 + $0",
            "foo",
            expect!["(LetBinding) let foo = 42; in 1 + foo"],
        );

        check(
            "{ foo, bar ? $0 }: 42",
            "foo",
            expect!["(Param) { foo, bar ? foo }: 42"],
        );
    }
}
