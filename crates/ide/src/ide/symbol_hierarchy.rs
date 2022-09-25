use crate::def::{BindingValue, Expr, ExprId};
use crate::{DefDatabase, FileId, Module, ModuleSourceMap, NameKind};
use rowan::ast::AstNode;
use rowan::TextRange;
use smol_str::SmolStr;
use syntax::{ast, SyntaxNode};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTree {
    pub name: SmolStr,
    pub full_range: TextRange,
    pub focus_range: TextRange,
    pub kind: NameKind,
    pub children: Vec<SymbolTree>,
}

pub(crate) fn symbol_hierarchy(db: &dyn DefDatabase, file: FileId) -> Vec<SymbolTree> {
    let parse = db.parse(file);
    let module = db.module(file);
    let source_map = db.source_map(file);
    let mut collector = Collector {
        module: &module,
        source_map: &source_map,
        root_node: parse.syntax_node(),
        symbols: Vec::new(),
    };
    collector.collect_expr(module.entry_expr());
    let mut symbols = collector.symbols;
    sort_symbols(&mut symbols);
    symbols
}

#[derive(Debug)]
struct Collector<'a> {
    module: &'a Module,
    source_map: &'a ModuleSourceMap,
    root_node: SyntaxNode,
    symbols: Vec<SymbolTree>,
}

impl Collector<'_> {
    fn collect_expr(&mut self, expr_id: ExprId) {
        let e = &self.module[expr_id];
        let (bindings, body) = match e {
            Expr::LetIn(bindings, body) => (bindings, Some(*body)),
            Expr::Attrset(bindings) | Expr::LetAttrset(bindings) | Expr::RecAttrset(bindings) => {
                (bindings, None)
            }
            _ => return e.walk_child_exprs(|child| self.collect_expr(child)),
        };

        for &(name, rhs) in bindings.statics.iter() {
            let prev_len = self.symbols.len();
            match rhs {
                BindingValue::InheritFrom(_) => {}
                BindingValue::Inherit(child) | BindingValue::Expr(child) => {
                    self.collect_expr(child)
                }
            }
            (|| {
                let text = self.module[name].text.clone();
                let kind = self.module[name].kind;
                let name_node = self.source_map.nodes_for_name(name).next()?;
                let focus_range = name_node.text_range();
                let full_range = name_node
                    .to_node(&self.root_node)
                    .ancestors()
                    .find_map(ast::Binding::cast)?
                    .syntax()
                    .text_range();
                let mut children = self.symbols.split_off(prev_len);
                sort_symbols(&mut children);
                self.symbols.push(SymbolTree {
                    name: text,
                    full_range,
                    focus_range,
                    kind,
                    children,
                });
                Some(())
            })();
        }

        bindings
            .dynamics
            .iter()
            .flat_map(|&(lhs, rhs)| [lhs, rhs])
            .chain(bindings.inherit_froms.iter().copied())
            .chain(body)
            .for_each(|e| self.collect_expr(e));
    }
}

fn sort_symbols(syms: &mut [SymbolTree]) {
    syms.sort_by_key(|sym| sym.full_range.start());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};
    use std::fmt::Write;

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, file) = TestDB::single_file(fixture).unwrap();
        let syms = symbol_hierarchy(&db, file);
        let mut got = String::new();
        fmt_symbols(0, &syms, &mut got);
        expect.assert_eq(&got);
    }

    fn fmt_symbols(indent: usize, syms: &[SymbolTree], out: &mut String) {
        for sym in syms {
            writeln!(
                out,
                "{:indent$}{}: {:?}",
                "",
                sym.name,
                sym.kind,
                indent = indent
            )
            .unwrap();
            fmt_symbols(indent + 4, &sym.children, out);
        }
    }

    #[test]
    fn let_in() {
        check(
            "let a.b = 1; c = let d = 1; in d; in a",
            expect![[r#"
                a: LetIn
                    b: PlainAttrset
                c: LetIn
                    d: LetIn
            "#]],
        );
    }

    #[test]
    fn attrset() {
        check(
            "{ a = 1; b = { c = 1; }; inherit d; inherit ({ e = 1; }) e; }",
            expect![[r#"
                a: PlainAttrset
                b: PlainAttrset
                    c: PlainAttrset
                d: PlainAttrset
                e: PlainAttrset
                e: PlainAttrset
            "#]],
        );
    }
}
