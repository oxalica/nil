nil: Language server of Nix Expression Language

🚧 *This project is under development, but be happy to try it out!*

Super fast incremental analysis! Scans `all-packages.nix` in less then 0.1s and completes with no delay!

## Features

- [x] Goto definition. `textDocument/definition`
- [x] Find references. `textDocument/reference`
- [x] Completion. `textDocument/completion`
  - [x] Builtin names.
  - [x] Local bindings.
  - [ ] Attrset fields.
- [ ] Cross-file analysis.
- [ ] Multi-threaded.

## Installation

1. Have the latest stable version of Rust installed.
2. `cargo install --git https://github.com/oxalica/nil.git`

### For neovim `nvim-lspconfig` user

Add the following vimscript to your configuration.

```vim
lua <<EOF
  require('lspconfig').rnix.setup {
    autostart = true,
    -- Set to the path to `nil` binary you installed.
    cmd = { vim.env.HOME .. "/.local/bin/nil" },
  }
EOF
```

## License

"nil" is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.
