use crate::def::{Expr, ExprId, Literal};
use crate::{DefDatabase, FileId, VfsPath};
use syntax::TextRange;
use url::Url;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Link {
    pub range: TextRange,
    pub tooltip: String,
    pub target: LinkTarget,
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
        let (tooltip, target) = match lit {
            Literal::String(s) => {
                let uri = try_resolve_link_uri(s)?;
                (uri.to_string(), LinkTarget::Uri(uri))
            }
            Literal::Path(p) => {
                let vpath = p.resolve(db)?;
                // Walkaround a lifetime issue.
                let tooltip = vpath.display().to_string();
                (tooltip, LinkTarget::VfsPath(vpath))
            }
            _ => return None,
        };
        Some(Link {
            range: source_map.node_for_expr(e)?.text_range(),
            tooltip,
            target,
        })
    };

    module.exprs().filter_map(extract_link).collect()
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
            .iter()
            .map(|link| {
                let target = match &link.target {
                    LinkTarget::Uri(uri) => uri.to_string(),
                    LinkTarget::VfsPath(p) => p.display().to_string(),
                };
                format!("{} -> {}: {}\n", &src[link.range], target, link.tooltip,)
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
