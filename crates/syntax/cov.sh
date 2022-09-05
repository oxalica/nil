#!/usr/bin/env bash
test() {
  RUSTFLAGS="-C instrument-coverage" \
    LLVM_PROFILE_FILE="cov/syntax-%m.profraw" \
    cargo test --tests
}

merge() {
  llvm-profdata merge -sparse cov/syntax-*.profraw -o cov/syntax.profdata
}

report() {
  local args=(--use-color --ignore-filename-regex='/registry/src/')
  local file
  while read -r file; do
    args+=(--object "$file")
  done < <(
    RUSTFLAGS="-C instrument-coverage" \
    cargo test --tests --no-run --message-format=json \
      | jq -r "select(.profile.test == true) | .filenames[]" \
      | grep -v dSYM -
  )
  args+=(--instr-profile=cov/syntax.profdata --summary-only)
  echo llvm-cov report "${args[@]}"
  llvm-cov report "${args[@]}"
}

show() {
  local exe_file="$(
    RUSTFLAGS="-C instrument-coverage" \
    cargo test --tests --no-run --message-format=json \
      | jq -r 'select(.executable) | .executable'
  )"
  llvm-cov show \
    "$exe_file" \
    --instr-profile=cov/syntax.profdata \
    --show-line-counts-or-regions \
    --show-instantiations \
    --use-color
}

"$@"
