use crate::def::{AstPtr, Expr, ExprId, Literal};
use crate::{DefDatabase, FileId, FileRange, VfsPath};
use syntax::TextRange;
use url::Url;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Link {
    Lazy {
        range: TextRange,
    },
    Resolved {
        range: TextRange,
        tooltip: String,
        target: LinkTarget,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LinkTarget {
    Uri(Url),
    VfsPath(VfsPath),
}

pub(crate) fn links(db: &dyn DefDatabase, file_id: FileId) -> Vec<Link> {
    let module = db.module(file_id);
    let source_map = db.source_map(file_id);

    let extract_link = |(e, kind): (ExprId, &Expr)| -> Option<Link> {
        let Expr::Literal(lit) = kind else { return None };
        let range = || Some(source_map.node_for_expr(e)?.text_range());
        match lit {
            Literal::String(s) => {
                let uri = try_resolve_link_uri(s)?;
                Some(Link::Resolved {
                    range: range()?,
                    tooltip: uri.as_str().to_owned(),
                    target: LinkTarget::Uri(uri),
                })
            }
            Literal::Path(_) => Some(Link::Lazy { range: range()? }),
            _ => None,
        }
    };

    module.exprs().filter_map(extract_link).collect()
}

pub(crate) fn link_resolve(db: &dyn DefDatabase, frange: FileRange) -> Option<Link> {
    let module = db.module(frange.file_id);
    let source_map = db.source_map(frange.file_id);
    let parse = db.parse(frange.file_id);

    let n = parse
        .syntax_node()
        .token_at_offset(frange.range.start())
        .right_biased()?
        .parent()?;
    let expr = source_map.expr_for_node(AstPtr::new(&n))?;
    let Expr::Literal(Literal::Path(path)) = &module[expr] else { return None };
    let vpath = path.resolve(db)?;
    // Workaround: inlining this causes lifetime issues.
    let tooltip = vpath.display().to_string();
    Some(Link::Resolved {
        range: frange.range,
        tooltip,
        target: LinkTarget::VfsPath(vpath),
    })
}

fn try_resolve_link_uri(uri: &str) -> Option<Url> {
    // We intentionally don't allow any string parsable as URI, mainly for:
    // 1. Efficiency.
    // 2. Less false-positives.
    // 3. Reject not-really-absolute `mirror:` URIs.
    const FILTERS: &[&str] = &[
        // Common URLs.
        "https:",
        "http:",
        "ftp:",
        "file:",
        // Flake-refs.
        // https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#types
        "path:",
        "git+",
        "tarball+",
        "github:",
        "sourcehut:",
    ];

    if !FILTERS.iter().any(|prefix| uri.starts_with(prefix)) {
        return None;
    }

    let mut uri = Url::parse(uri).ok()?;
    let scheme = uri.scheme();

    // Shortcut `github:owner/repo(/ref_or_rev)?`.
    if scheme == "github" {
        let mut iter = uri.path().splitn(3, '/');
        let owner = iter.next()?;
        let repo = iter.next()?;
        // let rev = iter.next()?; // TODO
        return Some(
            format!("https://github.com/{owner}/{repo}")
                .parse()
                .unwrap(),
        );
    }

    // Shortcut `sourcehut:owner/repo(/ref_or_rev)?`.
    if scheme == "sourcehut" {
        let mut iter = uri.path().splitn(3, '/');
        let owner = iter.next()?;
        let repo = iter.next()?;
        // let rev = iter.next()?; // TODO
        return Some(format!("https://sr.ht/{owner}/{repo}").parse().unwrap());
    }

    // For `anything+(file|path)://...`, chop it to `file://`.
    // Same for `https` and `http`.
    if scheme.contains("file") || scheme.contains("path") {
        uri.set_scheme("file").ok()?;
    } else if scheme.contains("https") {
        uri.set_scheme("https").ok()?;
    } else if scheme.contains("http") {
        uri.set_scheme("http").ok()?;
    }

    // Trim `?` and `#` parts, which are special for flake-ref.
    // Not a part of original URI.
    uri.set_query(None);
    uri.set_fragment(None);

    Some(uri)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let file_id = f.files()[0];
        let links = links(&db, file_id);
        let src = db.file_content(file_id);
        let got = links
            .into_iter()
            .filter_map(|link| {
                let (range, tooltip, target) = match link {
                    Link::Resolved {
                        range,
                        tooltip,
                        target,
                    } => (range, tooltip, target),
                    Link::Lazy { range } => {
                        match link_resolve(&db, FileRange::new(file_id, range))? {
                            Link::Lazy { .. } => unreachable!(),
                            Link::Resolved {
                                range,
                                tooltip,
                                target,
                            } => (range, tooltip, target),
                        }
                    }
                };
                let target = match &target {
                    LinkTarget::Uri(uri) => uri.to_string(),
                    LinkTarget::VfsPath(p) => p.display().to_string(),
                };
                let src = &src[range];
                Some(format!("{src} -> {target}: {tooltip}\n"))
            })
            .collect::<String>();
        expect.assert_eq(&got);
    }

    #[test]
    fn uri() {
        check(
            r#"[
                github:NixOS/nixpkgs
                "github:NixOS/nixpkgs/nixos-22.05"
                "github:NixOS/nixpkgs/pull/190594/head"
                "file:///root"
                "https://example.com?foo=1#bar"
            ]"#,
            expect![[r#"
                github:NixOS/nixpkgs -> https://github.com/NixOS/nixpkgs: https://github.com/NixOS/nixpkgs
                "github:NixOS/nixpkgs/nixos-22.05" -> https://github.com/NixOS/nixpkgs: https://github.com/NixOS/nixpkgs
                "github:NixOS/nixpkgs/pull/190594/head" -> https://github.com/NixOS/nixpkgs: https://github.com/NixOS/nixpkgs
                "https://example.com?foo=1#bar" -> https://example.com/: https://example.com/
            "#]],
        );
    }

    // FIXME: Currently target existence check is done in LSP handlers.
    #[test]
    fn path() {
        check(
            r#"
#- /default.nix
[ ./. ./foo.nix /bar ]

#- /foo.nix
1

#- /bar/default.nix
1
            "#,
            expect![[r#"
                ./. -> /: /
                ./foo.nix -> /foo.nix: /foo.nix
            "#]],
        );
    }
}
