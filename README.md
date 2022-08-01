nil: Language server of Nix Expression Language

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
