# nil: NIx Language server

[![CI](https://github.com/oxalica/nil/actions/workflows/ci.yaml/badge.svg)](https://github.com/oxalica/nil/actions/workflows/ci.yaml)

An incremental analysis assistant for writing in Nix.

See [release notes](https://github.com/oxalica/nil/releases) for changelog between releases.

See [`docs/features.md`](docs/features.md) for an incomplete list of notable features currently
implemented or planned.

See [`docs/configuration.md`](docs/configuration.md) for all tunable configuration options.

## Installation

This program is available in [NixOS/nixpkgs](https://github.com/NixOS/nixpkgs) under attribute `nil`,
and is regularly updated.

- If you use `nix-env`, run `nix-env -iA nixpkgs.nil`
- If you use `nix profile`, run `nix profile install nixpkgs#nil`
- If you want to compile it from source:
  1. Install stable Rust toolchain >= 1.70
  1. Install nix >= 2.4 and make sure the binary `nix` is in your `PATH`.
  1. Build and install via `cargo install --git https://github.com/oxalica/nil nil`

## Install with [Flake](https://nixos.wiki/wiki/Flakes)

This repo is also packaged via Nix flakes. The language server package is
available in the default flake output `github:oxalica/nil#`, under `bin/nil`.

To install, run `nix profile install github:oxalica/nil`. Alternatively,
you can use this repository as a flake input, and add its output to your own flake-managed
system-wide and/or home configurations.

*Disclaimer: The `flake.lock` we ship is tested in CI. If you use `follows` to
override flake inputs, we do not guarantee that it will build.*

Flake output structure (not necessarily up-to-date):
```
├───devShells
│   └───(...)
└───packages
    ├───x86_64-linux
    │   ├───default: package 'nil-unstable-2022-08-04'
    │   └───nil: package 'nil-unstable-2022-08-04'
    └───(...)
```

## Editor integration

### Neovim native LSP and [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)

We are officially supported by nvim-lspconfig, see [upstream docs](https://github.com/neovim/nvim-lspconfig/blob/0fafc3ef648bd612757630097c96b725a36a0476/doc/server_configurations.txt#nil_ls),
also [the example config for testing](dev/nvim-lsp.nix).

:warning: There is a known performance issue for semantic highlighting with
neovim native LSP. See more details in https://github.com/oxalica/nil/issues/83

### Vim/Neovim with [coc.nvim](https://github.com/neoclide/coc.nvim)

Merge this setting into your `coc-settings.json` (open with `:CocConfig`).

```jsonc
{
  "languageserver": {
    "nix": {
      "command": "nil",
      "filetypes": ["nix"],
      "rootPatterns":  ["flake.nix"],
      // Uncomment these to tweak settings.
      // "settings": {
      //   "nil": {
      //     "formatting": { "command": ["nixpkgs-fmt"] }
      //   }
      // }
    }
  }
}
```

See [the example config for testing](dev/vim-coc.nix).

### Vim with [vim-lsp](https://github.com/prabirshrestha/vim-lsp)

Add the following code to your `~/.vimrc` to register the LSP server.
Thanks @mitchmindtree

```vim
if executable('nil')
  autocmd User lsp_setup call lsp#register_server({
    \ 'name': 'nil',
    \ 'cmd': {server_info->['nil']},
    \ 'whitelist': ['nix'],
    \ })
endif
```

### Emacs with [lsp-mode](https://github.com/emacs-lsp/lsp-mode)

Add the following elisp code to your configuration. (using `use-package`)

```elisp
(use-package lsp-mode
  :ensure t)

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)
```

There are various other configurations to tweak. Refer to the
[specific manual page](https://emacs-lsp.github.io/lsp-mode/page/lsp-nix-nil/) for more details.

### Emacs with [eglot](https://github.com/joaotavora/eglot)

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

### VSCode/VSCodium with [Nix IDE](https://github.com/nix-community/vscode-nix-ide)

Modify the extension's settings in your `settings.json`.

```jsonc
{
  "nix.enableLanguageServer": true, // Enable LSP.
  "nix.serverPath": "nil" // The path to the LSP server executable.

  // Uncomment these to tweak settings.
  // "nix.serverSettings": {
  //   "nil": {
  //     "formatting": { "command": ["nixpkgs-fmt"] }
  //   }
  // }
}
```

### Kate with [LSP Client Plugin](https://docs.kde.org/stable5/en/kate/kate/kate-application-plugin-lspclient.html)

Add this to your "User Server Settings" in LSP Client configuration:

```json
{
  "servers": {
    "nix": {
      "command": ["nil"],
      "url": "https://github.com/oxalica/nil",
      "highlightingModeRegex": "^Nix$"
    }
  }
}
```

## License

"nil" is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.
