## LSP Features

When `nil` is invoked without arguments, it runs in the [LSP] mode.
Stdin and stdout are used for jsonrpc.

[LSP]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification

This incomplete list tracks noteble features currently implemented or planned.

- [x] Goto definition. `textDocument/definition`
  - [x] References to parameters, `let` and `rec {}` bindings.
  - [x] Relative paths.
  - [x] Source of flake inputs, when cursor is on keys of `inputs` or
    parameters of `outputs` lambda.
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

- [x] Code actions. `textDocument/codeAction`
  See [`docs/code_actions.md`](./code_actions.md) for the list of supported code actions.

- [x] Completion. `textDocument/completion`
  - [x] Builtin names.
    - With documentations.
  - [x] Local bindings and rec-attrset fields.
  - [x] Keywords.
  - [ ] Attrset fields.
    - [x] If it can be inferenced in the local file.
    - [x] Flake schema, including common inputs fields like `url` and
          output fields like `outPath`.
    - [ ] Real flake outputs from evaluation.
    - [x] NixOS options.
          Evaluated from the flake input named `nixpkgs`.
  - [x] Pat-parameter definition.
    - [x] Flake inputs in the parameter of `outputs`.

- [x] Diagnostics. `textDocument/publishDiagnostics`

  - [x] Syntax errors.
  - [x] Hard semantic errors reported as parse errors by Nix, like duplicated keys in attrsets.
  - [x] Undefined names.
  - [x] Warnings of legacy syntax.
  - [x] Warnings of unnecessary syntax.
  - [x] Warnings of unused bindings, `with` and `rec`.
  - [x] Warnings of unused parameters for packages, modules and flake output parameters.
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

  :warning: There is a known performance issue for semantic highlighting with
  neovim native LSP. See more details in https://github.com/oxalica/nil/issues/83

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

## CLI Features

`nil` could also be invoked in command line.
You can run `nil --help` for usages of all available commands.

- `nil diagnostics <PATH>`
  Check and print diagnostics for a file.
  Exit with code `1` if there are any errors.
  :warning: **WARNING**: The output format is for human and should not be relied on.
