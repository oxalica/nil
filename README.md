nil: Language server for Nix Expression Language

🚧 *This project is under development, but be happy to try it out!*

Super fast incremental analysis! Scans `all-packages.nix` in less than 0.1s and completes with no delay!

## Features

- [x] Goto definition. `textDocument/definition`
- [x] Find references. `textDocument/reference`
  - [x] Local binding references.
  - [x] With expression references.
- [x] Completion. `textDocument/completion`
  - [x] Builtin names.
  - [x] Local bindings.
  - [ ] Attrset fields.
- [x] Diagnostics. `textDocument/publishDiagnostics`
  - Syntax errors. 
    - Incomplete syntax errors are currently suppressed to avoid noisy outputs during typing.
  - [x] Hard semantic errors reported as parse errors by Nix, like duplicated keys in attrsets.
  - [x] Warnings of legacy syntax.
  - [x] Warnings of unnecessary syntax.
  - [ ] Client pulled diagnostics.
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

### For neovim `nvim-lspconfig` user

Add the following vimscript to your configuration.

```vim
lua <<EOF
  require('lspconfig').rnix.setup {
    autostart = true,
    -- Ensure `nil` is in your PATH.
    cmd = { "nil" },
  }
EOF
```

### For emacs `eglot` user

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
