# This file provides a configured neovim for debugging the LSP.
# Run `nvim-test` inside the shell to test.
# Env vars:
# - `NIL_PATH`: The path to "nil" LSP binary. Default: `target/debug/nil`
# - `NIL_LOG_PATH`: Where to redirect LSP's stderr. Default: `/tmp/nil.log`
{ pkgs ? import <nixpkgs> { } }:
let
  neovim = (pkgs.neovim.override {
    withPython3 = false;

    configure = {
      customRC = ''
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
        # https://github.com/neovim/nvim-lspconfig/pull/2053
        (nvim-lspconfig.overrideAttrs (old: {
          version = "pr-2053";
          src = pkgs.fetchFromGitHub {
            owner = "neovim";
            repo = "nvim-lspconfig";
            rev = "e094ff79c6f5b806f601c930d37870988bb5feaa";
            hash = "sha256-50sXmJMb7MUPWwtE/tt3neP3TNmC2guvMzJqeS4Tp98=";
          };
        }))
      ];
    };
  }).overrideAttrs (old: {
    buildCommand = old.buildCommand + ''
      mv $out/bin/nvim{,-test}
    '';
  });

  # lua
  luaRc = ''
    -- Make it easier to use.
    vim.o.mouse = 'a'
    vim.o.number = true
    vim.o.cursorline = true

    vim.o.laststatus = 2 -- Always show statusline.
    vim.o.statusline =
      '%<%f %m%r%y %LL ' ..
      '%=' ..
      ' 0x%-4.B %-16.(%lL,%cC%V,%oB%) %P'

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
      { 'gD', 'vim.lsp.buf.declaration()' },
      { 'gd', 'vim.lsp.buf.definition()' },
      { 'gi', 'vim.lsp.buf.implementation()' },
      { 'gr', 'vim.lsp.buf.references()' },
      { '[d', 'vim.diagnostic.goto_prev()' },
      { ']d', 'vim.diagnostic.goto_next()' },
      { '<space><space>', 'vim.lsp.buf.hover()' },
      { '<space>s', 'vim.lsp.buf.signature_help()' },
      { '<space>r', 'vim.lsp.buf.rename()' },
      { '<space>a', 'vim.lsp.buf.code_action()' },
      { '<space>d', 'vim.diagnostic.open_float()' },
      { '<space>q', 'vim.diagnostic.setloclist()' },
    }
    for i, lr in pairs(lsp_mappings) do
      vim.api.nvim_set_keymap('n', lr[1], '<cmd>lua ' .. lr[2] .. '<cr>', { noremap = true, silent = true })
    end

    -- https://github.com/neovim/nvim-lspconfig/wiki/Autocompletion
    local caps = vim.lsp.protocol.make_client_capabilities()
    caps = require('cmp_nvim_lsp').update_capabilities(caps)

    local lsp_path = vim.env.NIL_PATH or 'target/debug/nil'
    local log_path = vim.env.NIL_LOG_PATH or '/tmp/nil.log'
    require('lspconfig').nil_ls.setup {
      autostart = true,
      capabilities = caps,
      cmd = {
        '${pkgs.bash}/bin/bash', '-c',
        vim.fn.shellescape(lsp_path) .. ' 2>>' .. vim.fn.shellescape(log_path),
      },
    }
  '';

in
  neovim
