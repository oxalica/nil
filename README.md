nil: Language server for Nix Expression Language

🚧 *This project is under development, but be happy to try it out!*

Super fast incremental analysis! Scans `all-packages.nix` in less then 0.1s and completes with no delay!

## Features

- [x] Goto definition. `textDocument/definition`
- [x] Find references. `textDocument/reference`
  - [x] Local binding references.
  - [x] With expression references.
- [x] Completion. `textDocument/completion`
  - [x] Builtin names.
  - [x] Local bindings.
  - [ ] Attrset fields.
- [ ] Cross-file analysis.
- [ ] Multi-threaded.

## Installation

**TODO: Beginner friendly instructions.**

This repo is packaged via [Nix flakes][nix-flakes], the language server binary package is
available through the default flake output `github:oxalica/nil#` with the path `bin/nil`.

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

### For neovim `nvim-lspconfig` user

Add the following vimscript to your configuration.

```vim
lua <<EOF
  require('lspconfig').rnix.setup {
    autostart = true,
    -- Change it to the path to the `nil` binary you installed.
    cmd = { "/run/current-system/sw/bin/nil" },
  }
EOF
```

## License

"nil" is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.
