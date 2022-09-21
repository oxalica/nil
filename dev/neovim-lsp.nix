# This file provides a configured neovim for debugging the LSP.
# Run `nvim-test` inside the shell to test.
# Env vars:
# - `NIL_PATH`: The path to "nil" LSP binary. Default: `target/debug/nil`
{ pkgs ? import <nixpkgs> { } }:
let
  neovim = pkgs.neovim.override {
    configure = {
      customRC = ''
        source ${./vimrc.vim}
        lua <<EOF
        ${luaRc}
        EOF
      '';

      packages.myPlugins.start = with pkgs.vimPlugins; [
        vim-nix # File type and syntax highlighting.
        luasnip
        nvim-cmp
        cmp_luasnip
        cmp-nvim-lsp
        nvim-lspconfig
      ];
    };
  };

  # lua
  luaRc = ''
    local cmp = require('cmp')
    cmp.setup {
      snippet = {
        expand = function(args)
          require('luasnip').lsp_expand(args.body)
        end,
      },
      mapping = {
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<tab>'] = cmp.mapping.confirm { select = true },
      },
      sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
      }),
    }

    local lsp_mappings = {
      { 'gD', vim.lsp.buf.declaration },
      { 'gd', vim.lsp.buf.definition },
      { 'gi', vim.lsp.buf.implementation },
      { 'gr', vim.lsp.buf.references },
      { '[d', vim.diagnostic.goto_prev },
      { ']d', vim.diagnostic.goto_next },
      { ' ' , vim.lsp.buf.hover },
      { ' s', vim.lsp.buf.signature_help },
      { ' r', vim.lsp.buf.rename },
      { ' a', vim.lsp.buf.code_action },
      { ' d', vim.diagnostic.open_float },
      { ' q', vim.diagnostic.setloclist },
    }
    for i, map in pairs(lsp_mappings) do
      vim.keymap.set('n', map[1], function() map[2]() end)
    end

    -- https://github.com/neovim/nvim-lspconfig/wiki/Autocompletion
    local caps = vim.lsp.protocol.make_client_capabilities()
    caps = require('cmp_nvim_lsp').update_capabilities(caps)

    local lsp_path = vim.env.NIL_PATH or 'target/debug/nil'
    require('lspconfig').nil_ls.setup {
      autostart = true,
      capabilities = caps,
      cmd = { lsp_path },
      settings = {
        ['nil'] = {
          testSetting = 42,
        },
      },
    }
  '';

in pkgs.runCommand "nvim-lsp" { } ''
  mkdir -p $out/bin
  ln -s ${neovim}/bin/nvim $out/bin/nvim-lsp
''
