use crate::def::{AstPtr, NameId};
use crate::{DefDatabase, FileId, Module, ModuleSourceMap, NameKind};
use smol_str::SmolStr;
use syntax::ast::{self, AstNode};
use syntax::rowan::WalkEvent;
use syntax::{SyntaxNode, TextRange};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTree {
    pub name: SmolStr,
    // TODO: Avoid saving `NameId` in the public API.
    name_id: NameId,
    pub full_range: TextRange,
    pub focus_range: TextRange,
    pub kind: NameKind,
    pub children: Vec<SymbolTree>,
}

pub(crate) fn symbol_hierarchy(db: &dyn DefDatabase, file: FileId) -> Vec<SymbolTree> {
    let parse = db.parse(file);
    let module = db.module(file);
    let source_map = db.source_map(file);
    let mut symbols = Vec::new();
    let mut collector = Collector {
        module: &module,
        source_map: &source_map,
        symbols: &mut symbols,
    };
    collector.collect_node(&parse.syntax_node());
    symbols
}

#[derive(Debug)]
struct Collector<'a, 'b> {
    module: &'a Module,
    source_map: &'a ModuleSourceMap,
    symbols: &'b mut Vec<SymbolTree>,
}

impl Collector<'_, '_> {
    fn push_symbol(&mut self, name_id: NameId, attr: &ast::Attr, full_range: TextRange) {
        self.symbols.push(SymbolTree {
            name: self.module[name_id].text.clone(),
            name_id,
            full_range,
            focus_range: attr.syntax().text_range(),
            kind: self.module[name_id].kind,
            children: Vec::new(),
        });
    }

    // TODO: Rewrite this in non-recursive form?
    // Nested mutable borrowing is hard to impl without recursion yet.
    fn collect_node(&mut self, n: &SyntaxNode) {
        let mut iter = n.preorder();
        let mut last_is_path_value = false;
        while let Some(event) = iter.next() {
            let n = match event {
                WalkEvent::Enter(n) => n,
                WalkEvent::Leave(n) => {
                    last_is_path_value = ast::AttrpathValue::can_cast(n.kind());
                    continue;
                }
            };
            let Some(binding) = ast::Binding::cast(n) else {
                continue;
            };
            match binding {
                ast::Binding::Inherit(i) => {
                    for attr in i.attrs() {
                        let ptr = AstPtr::new(attr.syntax());
                        if let Some(name_id) = self.source_map.name_for_node(ptr) {
                            self.push_symbol(name_id, &attr, i.syntax().text_range());
                        }
                    }
                    // Continue traversing the from-expr. Attrs should be skipped automatically.
                }
                ast::Binding::AttrpathValue(path_value) => {
                    let Some(path) = path_value.attrpath() else {
                        continue;
                    };
                    iter.skip_subtree();
                    self.collect_path_value(path_value, path.attrs(), last_is_path_value);
                }
            }
        }
    }

    fn collect_path_value(
        &mut self,
        binding: ast::AttrpathValue,
        mut attrs: ast::AstChildren<ast::Attr>,
        allow_merge_to_last: bool,
    ) {
        let Some(attr) = attrs.next() else {
            if let Some(n) = binding.value() {
                self.collect_node(n.syntax());
            }
            return;
        };

        let ptr = AstPtr::new(attr.syntax());
        if let Some(name_id) = self.source_map.name_for_node(ptr) {
            let binding_end_pos = binding.syntax().text_range().end();

            // Merge adjacent bindings to the same tree node.
            // Eg. `{ a.b = 1; a.c = 2; }`
            //        ^-------a-------^
            let current_sym = if allow_merge_to_last
                && self.symbols.last().map(|tree| tree.name_id) == Some(name_id)
            {
                let last_sym = self.symbols.last_mut().unwrap();
                last_sym.full_range = last_sym.full_range.cover_offset(binding_end_pos);
                last_sym
            } else {
                // `{ foo.bar = 1; }`
                //        ---      focus
                //        ======== full
                let full_range = attr.syntax().text_range().cover_offset(binding_end_pos);
                self.push_symbol(name_id, &attr, full_range);
                self.symbols.last_mut().unwrap()
            };

            Collector {
                module: self.module,
                source_map: self.source_map,
                symbols: &mut current_sym.children,
            }
            .collect_path_value(binding, attrs, allow_merge_to_last);
        } else {
            // Incomplete or dynamic attributes.
            self.collect_node(attr.syntax());
            self.collect_path_value(binding, attrs, false);
        }
    }
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
    fn attrset_simple() {
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

    #[test]
    fn attrset_merge() {
        check(
            "{
                a.x.y = 1;
                # comment
                a.y.z = 2;
                another = 42;
                a.z = 3;
                inherit inhrit;
                a.w = 4;
            }",
            expect![[r#"
                a: PlainAttrset
                    x: PlainAttrset
                        y: PlainAttrset
                    y: PlainAttrset
                        z: PlainAttrset
                another: PlainAttrset
                a: PlainAttrset
                    z: PlainAttrset
                inhrit: PlainAttrset
                a: PlainAttrset
                    w: PlainAttrset
            "#]],
        );
    }
}
