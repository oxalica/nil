{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  codium = vscode-with-extensions.override {
    vscode = vscodium;
    vscodeExtensions = [ vscode-extensions.jnoortheen.nix-ide ];
  };
in writeShellScriptBin "codium-test" ''
  set -e
  dir="''${XDG_CACHE_HOME:-~/.cache}/nil-test/codium"
  ${coreutils}/bin/mkdir -p "$dir/User"
  cat >"$dir/User/settings.json" <<EOF
{
  "security.workspace.trust.enabled": false,
  "nix.enableLanguageServer": true,
  "nix.serverPath": "$NIL_PATH",
}
EOF
  exec ${codium}/bin/codium --user-data-dir "$dir" "$@"
''
