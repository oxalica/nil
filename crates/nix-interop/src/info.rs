//! Various information about Nix itself and environment.
use std::path::Path;

use anyhow::Result;
use serde::Deserialize;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct NixInfo {
    /// The version string reported by `builtins.nixVersion`.
    /// Note that this does not follow semver.
    pub version: String,
    /// Flake support.
    /// Requires nix >= 2.4
    pub flake: bool,
    /// Support for `--all-systems` and reduced output of `--legacy`.
    /// Requires nix >= 2.14
    pub flake_show_filter_systems: bool,
}

pub async fn get(nix_command: &Path) -> Result<NixInfo> {
    crate::eval::nix_eval_expr_json(
        nix_command,
        r#"
let
    inherit (builtins) nixVersion compareVersions;
    atLeast = v: compareVersions nixVersion v >= 0;
in {
    version = nixVersion;
    flake = atLeast "2.4";
    flake_show_filter_systems = atLeast "2.14";
}
        "#,
    )
    .await
}

#[cfg(test)]
mod tests {
    #[tokio::test]
    #[ignore = "requires calling 'nix'"]
    async fn simple() {
        let info = super::get("nix".as_ref()).await.unwrap();
        assert!(info.flake);
    }
}
