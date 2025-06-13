#!/usr/bin/env bash
set -eu
# Requires: [ git nixfmt-rfc-style fd typos ], rust toolchain, npm toolchain

die() { echo "$*" >&2; exit 1; }

if git_dir="$(git rev-parse --show-toplevel)"; then
  cd "$git_dir"
fi

typos || die "typos"

cargo fmt --all --check \
  || die 'cargo fmt failed'
fd -e nix --exclude=crates/syntax/test_data --exec-batch nixfmt --check \
  || die 'nixfmt failed'
cargo clippy --workspace --all-targets -- -Dwarnings \
  || die 'clippy failed'

cd editors/coc-nil && npm run lint
