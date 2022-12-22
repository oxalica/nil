# nil: NIx Language server

[![CI](https://github.com/oxalica/nil/actions/workflows/ci.yml/badge.svg)](https://github.com/oxalica/nil/actions/workflows/ci.yml)

An incremental analysis assistent for writing in Nix.

See [release notes][releases] for change log between tagged unstable versions.

See [`docs/features.md`](docs/features.md) for an incomplete list of notable features currently
implemented or planned.

See [`docs/configuration.md`](docs/configuration.md) for all tunable configuration options.

[releases]: https://github.com/oxalica/nil/releases

## Installation

This program is already included in [NixOS/nixpkgs][nixpkgs] under attribute `nil`,
and are regularly updated.

[nixpkgs]: https://github.com/NixOS/nixpkgs

- If you use `nix-env`, run `nix-env -iA nixpkgs.nil`
- If you use `nix profile`, run `nix profile install nixpkgs#nil`
- If you want to compile it from source.
  1. Install stable Rust toolchain >= 1.62
  1. Install `nix` >= 2.4 and make the binary `nix` available from your `PATH`.
  1. Build and install via `cargo install --git https://github.com/oxalica/nil nil`

## Flake

This repo is also packaged via [Nix flakes][nix-flakes], the language server binary package is
available through the default flake output `github:oxalica/nil#` with the path `bin/nil`.

You can [enable flakes support][nix-flakes-install] in your nix configuration, and then
run `nix profile install github:oxalica/nil` to get `nil` installed.
You can also use this repository as a flake input and add its output to your own flake-managed
systemwide or home configuration.

*Disclamer: We ship `flake.lock` which is tested in CI to be working. If you use `follows` to
override flake inputs, we provides no guarentee of whether it would still build.*

Flake output structure (not necessary up-to-date):
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

See also [the example config for testing](dev/nvim-lsp.nix).

### Vim/Neovim with [`coc.nvim`]

[`coc.nvim`]: https://github.com/neoclide/coc.nvim

Merge this setting into your `coc-settings.json`, which can be opened by `:CocConfig`.

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

See also [the example config for testing](dev/vim-coc.nix).

### Vim with [`vim-lsp`]

[`vim-lsp`]: https://github.com/prabirshrestha/vim-lsp

Add the following code to your `vimrc` to register the LSP server.
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

### Emacs with [`eglot`]

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

### Vscode/Vscodium with [Nix IDE]

[Nix IDE]: https://github.com/nix-community/vscode-nix-ide

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

### Kate with [LSP Client Plugin]

[LSP Client Plugin]: https://docs.kde.org/stable5/en/kate/kate/kate-application-plugin-lspclient.html

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
