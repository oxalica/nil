nil: Language server for Nix Expression Language

🚧 *This project is under development, but be happy to try it out!*

Super fast incremental analysis! Scans `all-packages.nix` in less than 0.1s and completes with no delay!

## Features

- [x] Goto definition. `textDocument/definition`
  - [x] References to parameters, `let` and `rec {}` bindings.
  - [x] Relative paths.
- [x] Find references. `textDocument/reference`
  - [x] Parameters, `let` and `rec {}` bindings.
  - [x] With expression.
- [x] Completion. `textDocument/completion`
  - [x] Builtin names.
  - [x] Local bindings and rec-attrset fields.
  - [x] Keywords.
  - [ ] Attrset fields.
- [x] Diagnostics. `textDocument/publishDiagnostics`
  - Syntax errors. 
    - Incomplete syntax errors are currently suppressed to avoid noisy outputs during typing.
  - [x] Hard semantic errors reported as parse errors by Nix, like duplicated keys in attrsets.
  - [x] Undefiend names.
  - [x] Warnings of legacy syntax.
  - [x] Warnings of unnecessary syntax.
  - [x] Warnings of unused bindings, `with` and `rec`.
  - [ ] Client pulled diagnostics.
- [x] Expand selection. `textDocument/selectionRange`
- [x] Renaming. `textDocument/renamme`, `textDocument/prepareRename`
  - [x] Identifiers in parameters and bindings, from `let`, rec and non-rec attrsets.
  - [x] Static string literal bindings.
  - [x] Merged path-value binding names.
  - [ ] Names introduced by `inherit`.
  - [ ] Names used by `inherit`.
  - [ ] Conflict detection.
  - [x] Rename to string literals.
- [x] Semantic highlighting. `textDocument/semanticTokens/{range,full}`
  - [ ] Delta response. `textDocument/semanticTokens/full/delta`
  - :warning: Currently it has performance issue in large files with [`coc.nvim`].
    `vim` would consume 100% CPU and is slow to respond when editing `all-packages.nix`.
    Though our LSP server's CPU usage is quite low.
    Other LSP clients are not tested.
- [ ] Cross-file analysis.
- [ ] Multi-threaded.

## Installation

This repo is packaged via [Nix flakes][nix-flakes], the language server binary package is
available through the default flake output `github:oxalica/nil#` with the path `bin/nil`.

You can [enable flakes support][nix-flakes-install] in your nix configuration, and then
run `nix profile install github:oxalica/nil` to get `nil` installed.
You can also use this repository as a flake input and add its output to your own flake-managed
systemwide or home configuration.

Flake output structure:
```
├───devShells
│   └───(...)
└───packages
    ├───x86_64-linux
    │   ├───default: package 'nil-unstable-2022-08-04'
    │   └───nil: package 'nil-unstable-2022-08-04'
    └───(...)
```

[nix-flakes]: https://nixos.wiki/wiki/Flakes
[nix-flakes-install]: https://nixos.wiki/wiki/Flakes#Installing_flakes

## Editor integration

### Neovim native LSP and [`nvim-lspconfig`]

[`nvim-lspconfig`]: https://github.com/neovim/nvim-lspconfig

We are officially supported by `nvim-lspconfig`, see [upstream docs](https://github.com/neovim/nvim-lspconfig/blob/0fafc3ef648bd612757630097c96b725a36a0476/doc/server_configurations.txt#nil_ls).

### Vim/Neovim via [`coc.nvim`]

[`coc.nvim`]: https://github.com/neoclide/coc.nvim

Merge this setting into your `coc-settings.json`, which can be opened by `:CocConfig`.

```json
{
  "languageserver": {
    "nix": {
      "command": "nil",
      "filetypes": ["nix"],
      "rootPatterns":  ["flake.nix"]
    }
  }
}
```

### Emacs [`eglot`]

[`eglot`]: https://github.com/joaotavora/eglot

Add the following elisp code to your configuration. (using `use-package`)

```elisp
(use-package nix-mode)
(use-package eglot
  :config
  ;; Ensure `nil` is in your PATH.
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  (nix-mode . eglot-ensure))
```

## License

"nil" is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.
