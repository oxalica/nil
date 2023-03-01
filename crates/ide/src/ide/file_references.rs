use nix_interop::DEFAULT_IMPORT_FILE;

use crate::def::{Expr, Literal};
use crate::{DefDatabase, FileId, NavigationTarget};

pub(crate) fn file_referrers(db: &dyn DefDatabase, file: FileId) -> Vec<NavigationTarget> {
    let source_root = db.source_root(db.file_source_root(file));
    let mut targets = Vec::new();
    for parent in db.module_referrers(file) {
        let module = db.module(parent);
        let source_map = db.source_map(parent);
        targets.extend(module.exprs().filter_map(|(expr_id, kind)| {
            // FIXME: This code dups with `module_references_query`.
            // We should rework the resolution of def::Path.
            let &Expr::Literal(Literal::Path(path)) = kind else { return None };
            let mut vpath = path.resolve(db)?;
            let target_file = source_root.file_for_path(&vpath).or_else(|| {
                vpath.push(DEFAULT_IMPORT_FILE)?;
                source_root.file_for_path(&vpath)
            })?;

            if target_file != file {
                return None;
            }
            let range = source_map.node_for_expr(expr_id)?.text_range();
            Some(NavigationTarget {
                file_id: parent,
                full_range: range,
                focus_range: range,
            })
        }));
    }
    targets
}
