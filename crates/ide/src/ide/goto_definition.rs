use super::NavigationTarget;
use crate::def::{AstPtr, BindingValue, Expr, Literal, ResolveResult};
use crate::{DefDatabase, FileId, FilePos, VfsPath};
use if_chain::if_chain;
use nix_interop::FLAKE_FILE;
use syntax::ast::{self, AstNode};
use syntax::{best_token_at_offset, match_ast, SyntaxKind, SyntaxToken};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GotoDefinitionResult {
    Path(VfsPath),
    Targets(Vec<NavigationTarget>),
}

pub(crate) fn goto_definition(
    db: &dyn DefDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<GotoDefinitionResult> {
    let parse = db.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;

    // Special case for goto flake inputs.
    if let Some(ret) = goto_flake_input(db, file_id, tok.clone()) {
        return Some(ret);
    }

    let ptr = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => Some(AstPtr::new(n.syntax())),
                ast::Name(n) => Some(AstPtr::new(n.syntax())),
                ast::Literal(n) => Some(AstPtr::new(n.syntax())),
                _ => None,
            }
        }
    })?;

    let source_map = db.source_map(file_id);
    let expr_id = source_map.expr_for_node(ptr)?;

    // Special case for goto-path.
    if tok.kind() == SyntaxKind::PATH {
        let module = db.module(file_id);
        let path = match &module[expr_id] {
            Expr::Literal(Literal::Path(path)) => path,
            _ => return None,
        };
        let path = path.resolve(db)?;
        return Some(GotoDefinitionResult::Path(path));
    }

    let name_res = db.name_resolution(file_id);
    let targets = match name_res.get(expr_id)? {
        &ResolveResult::Definition(name) => source_map
            .nodes_for_name(name)
            .filter_map(|ptr| {
                let name_node = ptr.to_node(&parse.syntax_node());
                let full_node = name_node.ancestors().find(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
                    )
                })?;
                Some(NavigationTarget {
                    file_id,
                    focus_range: name_node.text_range(),
                    full_range: full_node.text_range(),
                })
            })
            .collect(),
        ResolveResult::WithExprs(withs) => {
            withs
                .iter()
                .filter_map(|&with_expr| {
                    // with expr; body
                    // ^--^       focus
                    // ^--------^ full
                    let with_node = source_map
                        .node_for_expr(with_expr)
                        .expect("WithExprs must be valid")
                        .to_node(&parse.syntax_node());
                    let with_node = ast::With::cast(with_node).expect("WithExprs must be valid");
                    let with_token_range = with_node.with_token()?.text_range();
                    let with_header_end = with_node
                        .semicolon_token()
                        .map_or_else(|| with_node.syntax().text_range(), |tok| tok.text_range());
                    let with_header = with_token_range.cover(with_header_end);
                    Some(NavigationTarget {
                        file_id,
                        focus_range: with_token_range,
                        full_range: with_header,
                    })
                })
                .collect()
        }
        // Currently builtin names cannot "goto-definition".
        ResolveResult::Builtin(_) => return None,
    };

    Some(GotoDefinitionResult::Targets(targets))
}

fn goto_flake_input(
    db: &dyn DefDatabase,
    file: FileId,
    tok: SyntaxToken,
) -> Option<GotoDefinitionResult> {
    let flake_info = db.source_root_flake_info(db.file_source_root(file))?;
    if flake_info.flake_file != file {
        return None;
    }

    let ptr = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Attr(n) => Some(AstPtr::new(n.syntax())),
                _ => None,
            }
        }
    })?;

    let module = db.module(file);
    let source_map = db.source_map(file);
    let name_id = source_map.name_for_node(ptr)?;
    let name_str = &*module[name_id].text;

    let target_path = flake_info
        .input_store_paths
        .get(name_str)?
        .join_segment(FLAKE_FILE);

    // More reliable matching?
    let is_flake_input = (|| {
        if let Expr::Attrset(flake_set) = &module[module.entry_expr()] {
            if_chain! {
                if let Some(BindingValue::Expr(inputs_expr)) = flake_set.get("inputs", &module);
                if let Expr::Attrset(inputs) = &module[inputs_expr];
                if inputs.statics.iter().any(|&(input_name, _)| input_name == name_id);
                then {
                    return true;
                }
            }
            if_chain! {
                if name_str != "self";
                if let Some(BindingValue::Expr(outputs_expr)) = flake_set.get("outputs", &module);
                if let Expr::Lambda(_, Some(pat), _) = &module[outputs_expr];
                if pat.fields.iter().any(|&(pat_param, _)| pat_param == Some(name_id));
                then {
                    return true;
                }
            }
        }
        false
    })();

    is_flake_input.then_some(GotoDefinitionResult::Path(target_path))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check_no(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        assert_eq!(goto_definition(&db, f[0]), None);
    }

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        let mut got = match goto_definition(&db, f[0]).expect("No definition") {
            GotoDefinitionResult::Path(path) => format!("file://{}", path.as_str()),
            GotoDefinitionResult::Targets(targets) => {
                assert!(!targets.is_empty());
                targets
                    .into_iter()
                    .map(|target| {
                        assert!(target.full_range.contains_range(target.focus_range));
                        let src = db.file_content(target.file_id);
                        let mut full = src[target.full_range].to_owned();
                        let relative_focus = target.focus_range - target.full_range.start();
                        full.insert(relative_focus.end().into(), '>');
                        full.insert(relative_focus.start().into(), '<');
                        full
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            }
        };
        // Prettify.
        if got.contains('\n') {
            got += "\n";
        }
        expect.assert_eq(&got);
    }

    #[test]
    fn not_found() {
        check_no("$0a");
        check_no("b: $0a");
    }

    #[test]
    fn invalid_position() {
        check_no("1 $0+ 2");
        check_no("wi$0th 1; 2");
    }

    #[test]
    fn lambda_param() {
        check("a: (a: (a $0a)) 1", expect!["<a>: (a a)"]);
        check("x: (a: (a $0x)) 1", expect!["<x>: (a: (a x)) 1"]);
        check("a: (a@{ x }: (a $0a)) 1", expect!["<a>@{ x }: (a a)"]);
        check("a: ({ x ? $0a }@a: a) 1", expect!["{ x ? a }@<a>: a"]);
        check("a: ({ x ? $0x }@a: a) 1", expect!["{ <x> ? x }@a: a"]);
    }

    #[test]
    fn with_env() {
        check("with 1; let a = 1; in with 2; $0a", expect!["<a> = 1;"]);
        check(
            "with 1; let a = 1; in with 2; $0b",
            expect![[r#"
                <with> 2;
                <with> 1;
            "#]],
        );
    }

    #[test]
    fn bindings() {
        check(
            "let a = a; in rec { inherit a; b = $0a; }",
            expect!["inherit <a>;"],
        );
        check(
            "let a = a; in rec { inherit $0a; b = a; }",
            expect!["<a> = a;"],
        );
        check(
            "let a = $0a; in rec { inherit a; b = a; }",
            expect!["<a> = a;"],
        );
    }

    #[test]
    fn left_and_right() {
        check("let a = 1; in $0a ", expect!["<a> = 1;"]);
        check("let a = 1; in a$0 ", expect!["<a> = 1;"]);
        check("let a = 1; in 0+$0a+0", expect!["<a> = 1;"]);
        check("let a = 1; in 0+a$0+0", expect!["<a> = 1;"]);
    }

    #[test]
    fn merged_binding() {
        check(
            "let a.a = 1; a.b = 2; a = { c = 3; }; in $0a",
            expect![[r#"
                <a>.a = 1;
                <a>.b = 2;
                <a> = { c = 3; };
            "#]],
        );
        check(
            "rec { b = $0a; a = { a = 1; }; a = { a = 2; }; }",
            expect![[r#"
                <a> = { a = 1; };
                <a> = { a = 2; };
            "#]],
        );
    }

    #[test]
    fn builtin() {
        check("let true = 1; in $0true && false", expect!["<true> = 1;"]);
        check_no("let true = 1; in true && $0false");
    }

    #[test]
    fn path() {
        check("1 + $0./.", expect!["file:///"]);
        check(
            "
#- /default.nix
import $0./bar.nix

#- /bar.nix
hello
            ",
            expect!["file:///bar.nix"],
        );
    }

    #[test]
    fn flake_input() {
        check(
            r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee input:nix=/nix/store/oooo
{
    description = "Hello flake";
    inputs.$0nixpkgs.url = "github:NixOS/nixpkgs";
    inputs.nix.url = "github:NixOS/nix";
    output = { ... }: { };
}
            "#,
            expect!["file:///nix/store/eeee/flake.nix"],
        );

        // Flake input in string form.
        check(
            r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee input:nix=/nix/store/oooo
{
    description = "Hello flake";
    inputs = {
        nixpkgs = { url = "github:NixOS/nixpkgs"; };
        "n$0ix" = { url = "github:NixOS/nix"; };
    };
    output = { ... }: { };
}
            "#,
            expect!["file:///nix/store/oooo/flake.nix"],
        );

        // Not a flake input.
        check_no(
            r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee
{
    description = "Hello flake";
    inputs.nixpkgs.url = "github:NixOS/nixpkgs";
    inputs'.$0nixpkgs.no = 42;
}
            "#,
        );

        // Not a flake input.
        check(
            r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee
{
    description = "Hello flake";
    inputs.nixpkgs.url = "github:NixOS/nixpkgs";
    outputs = { nixpkgs, ... }: $0nixpkgs;
            "#,
            expect!["{ <nixpkgs>, ... }: nixpkgs"],
        );
    }

    #[test]
    fn flake_output_pat() {
        check(
            r#"
#- /flake.nix input:nixpkgs=/nix/store/eeee
{
    outputs = { $0nixpkgs, ... }: nixpkgs;
}
            "#,
            expect!["file:///nix/store/eeee/flake.nix"],
        );

        // `self` in outputs is always the current flake, not the intput one.
        check_no(
            r#"
#- /flake.nix input:self=/nix/store/eeee
{
    outputs = { $0self, ... }: self;
}
            "#,
        );
    }
}
