## Features

This incomplete list tracks noteble features currently implemented or planned.

- [x] Goto definition. `textDocument/definition`
  - [x] References to parameters, `let` and `rec {}` bindings.
  - [x] Relative paths.
- [x] Find references. `textDocument/reference`
  - [x] Parameters, `let` and `rec {}` bindings.
  - [x] With expression.
- [x] Highlight related. `textDocument/documentHighlight`.
  - [x] Highlight definitions and references when cursor's on identifiers.
  - [x] Highlight all (attribute) references when cursor's on `with`.
  - [x] Highlight all effective `with`s when cursor's on attributes from `with`.
- [x] Links. `textDocument/documentLink`
  - [x] Links for relative and absolute paths.
  - [ ] Links for search paths like `<nixpkgs>`.
  - [x] Links for URLs like `"https://..."`, `"http://..."` and etc.
  - [x] Links for [flake references][flake-ref] like `"github:NixOS/nixpkgs"`.

  :warning: Due to an issue in `coc.nvim`, a lot of links
  can cause performance issue for `coc.nvim`, especially when editing `all-packages.nix`.
  You could apply [this patch][coc-links-fix] to mitigate.

  [coc-links-fix]: https://github.com/neoclide/coc.nvim/pull/4401

- [x] Code actions. `textDocument/codeAction`
  - [x] Convert `name = name;` bindings to `inherit`.
  - [x] Pack multiple bindings into one Attrset binding.

    `{ foo.bar = 1; foo.baz = 2; }` => `{ foo = { bar = 1; baz = 2; }; }`

  - [x] Flatten Attrset into outer level bindings.

    `{ foo = { bar = 1; baz = 2; }; }` => `{ foo.bar = 1; foo.baz = 2; }` 

- [x] Completion. `textDocument/completion`
  - [x] Builtin names.
    - With documentations.
  - [x] Local bindings and rec-attrset fields.
  - [x] Keywords.
  - [ ] Attrset fields.

- [x] Diagnostics. `textDocument/publishDiagnostics`

  - [x] Syntax errors. 
  - [x] Hard semantic errors reported as parse errors by Nix, like duplicated keys in attrsets.
  - [x] Undefiend names.
  - [x] Warnings of legacy syntax.
  - [x] Warnings of unnecessary syntax.
  - [x] Warnings of unused bindings, `with` and `rec`.
  - [ ] Client pulled diagnostics.
  - [x] Custom filter on kinds.
  - [x] Exclude files.

  You can disable some diagnostic kinds or for some (generated) files via LSP configuration.
  See [docs/configuration.md](./configuration.md) for more information.

- [x] Expand selection. `textDocument/selectionRange`
- [x] Renaming. `textDocument/renamme`, `textDocument/prepareRename`
  - [x] Identifiers in parameters and bindings, from `let`, rec and non-rec attrsets.
  - [x] Static string literal bindings.
  - [x] Merged path-value binding names.
  - [x] Names introduced by `inherit`.
  - [x] Names used by `inherit`.
  - [ ] Conflict detection.
  - [x] Rename to string literals.
- [x] Semantic highlighting. `textDocument/semanticTokens/{range,full}`
  - [ ] Delta response. `textDocument/semanticTokens/full/delta`

  Note: [`coc.nvim`] doesn't enable semantic highlighting by default.
  You need to manually enable it in settings.
  ```jsonc
  // coc-settings.json
  {
    "semanticTokens": { "filetypes": ["nix"] }
  }
  ```

- [x] Hover text. `textDocument/hover`.
  - [x] Show kind of names.
  - [x] Documentation for builtin names.
- [x] File symbols with hierarchy (aka. outline). `textDocument/documentSymbol`

- [x] File formatting.
  - [x] Whole file formatting.
  - [ ] Range formatting.
  - [ ] On-type formatting.
  - [x] External formatter.

  External formatter must be manually configured to work.
  See [docs/configuration.md](./configuration.md) for more information.

  When formatter is configured, you can also enable format-on-save in your editor.
  Like, for [`coc.nvim`],
  ```jsonc
  // coc-settings.json
  {
    "coc.preferences.formatOnSaveFiletypes": ["nix"]
  }
  ```

- [ ] Cross-file analysis.
- [x] Multi-threaded.
  - [x] Request cancellation. `$/cancelRequest`

[`coc.nvim`]: https://github.com/neoclide/coc.nvim
[flake-ref]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#types
