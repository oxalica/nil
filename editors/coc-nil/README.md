# coc-nil

**WIP**

[nil] language server glue for Vim/Neovim, works as an extension with [coc.nvim].

[nil]: https://github.com/oxalica/nil
[coc.nvim]: https://github.com/neoclide/coc.nvim

## Install

`:CocInstall coc-nil`

## Configurations

This extension is configured using a jsonc file. You can open this configuration file using the command `:CocConfig`,
and it is typically located at `$HOME/.config/nvim/coc-settings.json`.

Configurations to the LSP is documentated in
[`docs/configuration.md` of the LSP repository](https://github.com/oxalica/nil/blob/main/docs/configuration.md#reference),
with the exception that configuration keys can **ALSO** be written as flattened dot-separated string key.
That is, `coc-nvim` supports `"nil.nix.binary": "nix"`, `"nil": { "nix": { "binary": "nix" } }"`,
and even `"nil": { "nix.binary": "nix" }`.

The table below shows all extra configurations for the extension itself.

| Configuration | Description | Default |
|---|---|---|
| `nil.enable` | Enable `coc-nil` | `true` |
| `nil.server.path` | Path to the `nil` LSP server | `"nil"` |

## License

"coc-nil" is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.
