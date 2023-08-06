#[cfg(test)]
macro_rules! define_check_assist {
    ($handler:path) => {
        #[track_caller]
        fn check(fixture: &str, expect: ::expect_test::Expect) {
            crate::ide::assists::tests::check_assist($handler, fixture, expect);
        }
        #[track_caller]
        #[allow(dead_code)]
        fn check_no(fixture: &str) {
            crate::ide::assists::tests::check_assist_no($handler, fixture);
        }
    };
}

mod add_to_top_level_lambda_param;
mod convert_to_inherit;
mod flatten_attrset;
mod pack_bindings;
mod remove_empty_inherit;
mod remove_empty_let_in;
mod rewrite_string;

use crate::{DefDatabase, FileRange, TextEdit, WorkspaceEdit};
use syntax::ast::{self, AstNode};
use syntax::{best_token_at_offset, NixLanguage};

#[derive(Debug, Clone)]
pub struct Assist {
    /// Assist identifier.
    pub id: String,
    /// The label for human.
    pub label: String,
    pub kind: AssistKind,
    pub edits: WorkspaceEdit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssistKind {
    QuickFix,
    RefactorRewrite,
}

pub(crate) fn assists(db: &dyn DefDatabase, frange: FileRange) -> Vec<Assist> {
    let handlers = [
        add_to_top_level_lambda_param::add_to_top_level_lambda_param,
        convert_to_inherit::convert_to_inherit,
        flatten_attrset::flatten_attrset,
        pack_bindings::pack_bindings,
        remove_empty_inherit::remove_empty_inherit,
        remove_empty_let_in::remove_empty_let_in,
        rewrite_string::quote_attr,
        rewrite_string::rewrite_indented_to_string,
        rewrite_string::rewrite_string_to_indented,
        rewrite_string::rewrite_uri_to_string,
        rewrite_string::unquote_attr,
    ];

    let mut ctx = AssistsCtx::new(db, frange);
    for h in handlers {
        h(&mut ctx);
    }
    ctx.assists
}

pub(crate) struct AssistsCtx<'a> {
    db: &'a dyn DefDatabase,
    frange: FileRange,
    ast: ast::SourceFile,
    assists: Vec<Assist>,
}

impl<'a> AssistsCtx<'a> {
    fn new(db: &'a dyn DefDatabase, frange: FileRange) -> Self {
        AssistsCtx {
            db,
            frange,
            ast: db.parse(frange.file_id).root(),
            assists: Vec::new(),
        }
    }

    fn add(
        &mut self,
        id: impl Into<String>,
        label: impl Into<String>,
        kind: AssistKind,
        mut text_edits: Vec<TextEdit>,
    ) {
        text_edits.sort_unstable_by_key(|edit| edit.delete.start());
        let edits = WorkspaceEdit {
            content_edits: [(self.frange.file_id, text_edits)].into_iter().collect(),
        };
        self.assists.push(Assist {
            id: id.into(),
            label: label.into(),
            kind,
            edits,
        });
    }

    fn covering_node<N: AstNode<Language = NixLanguage>>(&self) -> Option<N> {
        let range = self.frange.range;
        if range.is_empty() {
            best_token_at_offset(self.ast.syntax(), range.start())?.into()
        } else {
            self.ast.syntax().covering_element(range)
        }
        .ancestors()
        .find_map(N::cast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestDB;
    use crate::SourceDatabase;
    use expect_test::Expect;

    #[track_caller]
    fn try_apply_assist(
        handler: fn(&mut AssistsCtx) -> Option<()>,
        fixture: &str,
    ) -> Option<String> {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.files().len(), 1);
        let frange = f.unwrap_single_range_marker();
        let mut ctx = AssistsCtx::new(&db, frange);
        handler(&mut ctx);

        let assist = ctx.assists.pop()?;
        let mut src = db.file_content(f[0].file_id).to_string();
        // Reverse apply.
        for edit in assist.edits.content_edits[&f[0].file_id].iter().rev() {
            edit.apply(&mut src);
        }
        // Don't count spaces at the end of lines.
        let mut src = src
            .lines()
            .flat_map(|line| [line.trim_end(), "\n"])
            .collect::<String>();
        if !src[..src.len() - 1].contains('\n') {
            src.pop();
        }
        Some(src)
    }

    #[track_caller]
    pub(crate) fn check_assist(
        handler: fn(&mut AssistsCtx) -> Option<()>,
        fixture: &str,
        expect: Expect,
    ) {
        let got = try_apply_assist(handler, fixture).expect("Not applicable");
        expect.assert_eq(&got);
    }

    #[track_caller]
    pub(crate) fn check_assist_no(handler: fn(&mut AssistsCtx) -> Option<()>, fixture: &str) {
        if let Some(got) = try_apply_assist(handler, fixture) {
            panic!("Unexpected applicable:\n{got}");
        }
    }
}
