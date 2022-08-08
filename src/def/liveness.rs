//! Liveness check of bindings and "with" expressions,
//! locating uncessary or inaccessible bindings and expressions.
//!
//! We only identify codes eliminable without any semantic change.
//! Thus names in LetIn are subjects of warnings, while fields from RecAttrset or Pat are not.
use la_arena::ArenaMap;

use super::{
    BindingKey, BindingValue, DefDatabase, Expr, ExprId, Module, ModuleScopes, NameDefId,
    ResolveResult,
};
use crate::FileId;
use std::sync::Arc;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LivenessCheck {
    unused_name_defs: Box<[NameDefId]>,
    unused_withs: Box<[ExprId]>,
    unused_recs: Box<[ExprId]>,
}

impl LivenessCheck {
    pub fn unused_name_defs(&self) -> &[NameDefId] {
        &self.unused_name_defs
    }

    pub fn unused_withs(&self) -> &[ExprId] {
        &self.unused_withs
    }

    pub fn unused_recs(&self) -> &[ExprId] {
        &self.unused_recs
    }

    pub(crate) fn liveness_check_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let module = db.module(file_id);
        let scopes = db.scopes(file_id);
        let mut traversal = Traversal::new(&module, &scopes);
        traversal.run(module.entry_expr);
        Arc::new(Self {
            unused_name_defs: traversal.unused_name_defs.into(),
            unused_withs: traversal.unused_withs.into(),
            unused_recs: traversal.unused_recs.into(),
        })
    }
}

struct Traversal<'a> {
    module: &'a Module,
    scopes: &'a ModuleScopes,
    def_expr_map: ArenaMap<NameDefId, ExprId>,
    visited_defs: ArenaMap<NameDefId, ()>,
    visited_def_exprs: ArenaMap<ExprId, ()>,
    visited_withs: ArenaMap<ExprId, ()>,
    queue: Vec<ExprId>,
    unused_name_defs: Vec<NameDefId>,
    unused_withs: Vec<ExprId>,
    unused_recs: Vec<ExprId>,
}

impl<'a> Traversal<'a> {
    fn new(module: &'a Module, scopes: &'a ModuleScopes) -> Self {
        // Preprocess dependencies of each NameDef.
        let mut def_expr_map: ArenaMap<NameDefId, ExprId> = ArenaMap::default();
        for (_, binding) in module.bindings() {
            // Here we only track dependencies, thus InheritFrom is the same as Inherit.
            if let (
                &BindingKey::NameDef(def),
                BindingValue::Inherit(e) | BindingValue::InheritFrom(e) | BindingValue::Expr(e),
            ) = (&binding.key, binding.value)
            {
                def_expr_map.insert(def, e);
            }
        }

        Self {
            module,
            scopes,
            def_expr_map,
            visited_defs: ArenaMap::default(),
            visited_def_exprs: ArenaMap::default(),
            visited_withs: ArenaMap::default(),
            queue: Vec::new(),
            unused_name_defs: Vec::new(),
            unused_withs: Vec::new(),
            unused_recs: Vec::new(),
        }
    }

    // Traverse live expressions and detect dead codes.
    fn run(&mut self, entry: ExprId) {
        // Traverse with BFS.
        self.queue.push(entry);
        while let Some(expr) = self.queue.pop() {
            match &self.module[expr] {
                Expr::Reference(name) => match self.scopes.resolve_name(expr, name) {
                    Some(ResolveResult::NameDef(def)) => {
                        self.visited_defs.insert(def, ());
                        if let Some(&e) = self.def_expr_map.get(def) {
                            // Bindings can be referenced in multiple places.
                            // We need to avoid re-entrance.
                            if self.visited_def_exprs.get(e).is_none() {
                                self.visited_def_exprs.insert(e, ());
                                self.queue.push(e);
                            }
                        }
                    }
                    Some(ResolveResult::WithExprs(exprs)) => {
                        for expr in exprs {
                            self.visited_withs.insert(expr, ());
                        }
                    }
                    Some(ResolveResult::Builtin(_)) | None => {}
                },
                // Don't walk LetIn bindings unless referenced.
                Expr::LetIn(_, body) => self.queue.push(*body),
                e => e.walk_child_exprs(self.module, |e| self.queue.push(e)),
            }
        }

        self.collect(entry);
    }

    /// Collect unused codes. But re-visit outmost unused bindings to avoid redandunt results.
    fn collect(&mut self, expr: ExprId) {
        let kind = &self.module[expr];

        match kind {
            Expr::LetIn(bindings, body) => {
                // Collect all errors first.
                bindings.walk_child_defs(self.module, |def, _| {
                    if self.visited_defs.get(def).is_none() {
                        self.unused_name_defs.push(def);
                    }
                });

                // Then recover from outermost Expr.
                bindings.walk_child_defs(self.module, |def, value| {
                    if self.visited_defs.get(def).is_none() {
                        let e = match *value {
                            BindingValue::Inherit(e)
                            | BindingValue::InheritFrom(e)
                            | BindingValue::Expr(e) => e,
                        };
                        self.run(e);
                    }
                });

                self.collect(*body);

                // Bindings are already re-collected! Don't repeat.
                return;
            }
            Expr::With(..) if self.visited_withs.get(expr).is_none() => {
                self.unused_withs.push(expr);
            }
            &Expr::Lambda(Some(param), Some(_), _) if self.visited_defs.get(param).is_none() => {
                self.unused_name_defs.push(param);
            }
            Expr::RecAttrset(bindings)
                if bindings
                    .entries
                    .iter()
                    .all(|&binding| match self.module[binding].key {
                        BindingKey::NameDef(def) => self.visited_defs.get(def).is_none(),
                        BindingKey::Name(_) | BindingKey::Dynamic(_) => true,
                    }) =>
            {
                self.unused_recs.push(expr);
            }
            _ => {}
        }

        // There may be more deep LetIn need collection.
        kind.walk_child_exprs(self.module, |e| self.collect(e));
    }
}

#[cfg(test)]
mod tests {
    use crate::def::DefDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, file, []) = TestDB::single_file(fixture).unwrap();
        let module = db.module(file);
        let source_map = db.source_map(file);
        let liveness = db.liveness_check(file);
        let got = liveness
            .unused_name_defs
            .iter()
            .map(|&def| {
                let pos = source_map.name_def_node(def).unwrap().text_range().start();
                format!("{}@{:?}", module[def].name, pos)
            })
            .chain(liveness.unused_withs.iter().map(|&expr| {
                let pos = source_map.expr_node(expr).unwrap().text_range().start();
                format!("with@{:?}", pos)
            }))
            .chain(liveness.unused_recs().iter().map(|&expr| {
                let pos = source_map.expr_node(expr).unwrap().text_range().start();
                format!("rec@{:?}", pos)
            }))
            .collect::<Vec<_>>()
            .join(" ");
        expect.assert_eq(&got);
    }

    #[test]
    fn let_in() {
        check("let a = 1; b = a; c = a; in c", expect!["b@11"]);
        check("let a = 1; in let a = 2; in a", expect!["a@4"]);
        check("let a = 1; in let inherit a; in a", expect![""]);
    }

    #[test]
    fn lambda() {
        check("a: { b }: c@{}: 0", expect!["c@10"]);
    }

    #[test]
    fn with() {
        check("a: with 1; a", expect!["with@3"]);
        check("a: with 1; with 2; b", expect![""]);
    }

    #[test]
    fn rec_attrset() {
        check("rec { a = 1; b = a; c = 1; }", expect![""]);
        check("rec { a = 1; b = 1; c = 1; }", expect!["rec@0"]);
        check("rec { }", expect!["rec@0"]);
    }

    #[test]
    fn no_recursive_unused() {
        check("let a = let b = 1; in b; in 1", expect!["a@4"]);
        check("let a = rec { b = b; }; in 1", expect!["a@4"]);
    }

    #[test]
    fn deeper_unused_use_outer_unused() {
        check(
            "let a = 1; b = let c = a; in 1; in 1",
            expect!["a@4 b@11 c@19"],
        );
    }
}
