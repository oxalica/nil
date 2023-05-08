{ pkgs ? import <nixpkgs> { } }:
let
  # vim
  customRC = ''
    source ${./vimrc.vim}

    if !empty($COC_NIL_PATH)
      execute 'set rtp^=' . $COC_NIL_PATH
    endif

    autocmd BufRead,BufNewFile *.nix setf nix

    let g:coc_node_path = '${pkgs.nodejs}/bin/node'
    let g:coc_config_home = '${cocConfigHome}'
    let g:coc_data_home = (empty($TMPDIR) ? '/tmp' : $TMPDIR) . '/coc-data'
    let leader = '\\'

    set updatetime=300
    " Color encoding.
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
    syntax on

    function! LspStatus()
      return coc#status()
    endfunction
    autocmd User CocStatusChange redrawstatus

    " Semantic highlighting.
    autocmd CursorHold * silent call CocActionAsync('highlight')

    inoremap <silent><expr> <TAB> coc#pum#visible() ? coc#pum#confirm() : "\<Tab>"
    inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
    inoremap <silent><expr> <c-@> coc#refresh()

    nmap <silent> [d <Plug>(coc-diagnostic-prev)
    nmap <silent> ]d <Plug>(coc-diagnostic-next)
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)
    nmap <silent> gl <Plug>(coc-openlink)

    nmap <silent> <C-s> <Plug>(coc-range-select)
    xmap <silent> <C-s> <Plug>(coc-range-select)

    nnoremap <silent> <Space><Space> <Cmd>call CocActionAsync('doHover')<CR>
    nnoremap <silent> <Space>s       <Cmd>call CocActionAsync('showSignatureHelp')<CR>

    nmap <silent> <Leader>r <Plug>(coc-rename)
    nmap <silent> <Leader>a <Plug>(coc-codeaction-cursor)
    xmap <silent> <Leader>a <Plug>(coc-codeaction-selected)
    nmap <silent> <Leader>qf <Plug>(coc-fix-current)

    command -nargs=0 CocShowOutput CocCommand workspace.showOutput languageserver.nix
    command -nargs=0 CocSemanticHighlightInfo call CocActionAsync('showSemanticHighlightInfo')

    " Workaround: https://github.com/EdenEast/nightfox.nvim/issues/236
    lua vim.treesitter = { highlighter = { hl_map = {} } }

    packadd! nightfox.nvim
    " https://github.com/EdenEast/nightfox.nvim/issues/218
    lua <<EOF
      require("nightfox").setup({
        options = {
          modules = {
            treesitter = true,
          },
        },
      })
    EOF
    colorscheme nightfox

    highlight link Identifier        TSVariable
    highlight link CocSemPath        Include
    highlight link CocSemVariable    TSVariable
    highlight link CocSemParameter   Identifier
    highlight link CocSemPunctuation TSOperator

    highlight link CocSemEscape     TSStringEscape
    highlight link CocSemUnresolved Error
    highlight link CocSemWithAttribute Underlined

    highlight link CocSemDelimiterPunctuation   TSPunctDelimiter
    highlight link CocSemConditionalKeyword     Conditional
    highlight link CocSemBuiltinVariable TSConstBuiltin
    highlight link CocSemBuiltinFunction TSFuncBuiltin
    highlight link CocSemBuiltin         TSVariableBuiltin
  '';

  cocSetting = {
    coc.preferences.formatOnSaveFiletypes = [ "nix" ];
    links.tooltip = true;
    semanticTokens.filetypes = [ "nix" ];

    nil.server.path = pkgs.writeShellScript "nil" ''
      exec "$NIL_PATH" "$@"
    '';
    nil.formatting.command = [ "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt" ];
    nil.diagnostics.excludedFiles = [ "generated.nix" ];
    nil.nix.flake.autoEvalInputs = true;
  };


  cocConfigHome = pkgs.writeTextFile {
    name = "coc-config";
    destination = "/coc-settings.json";
    text = builtins.toJSON cocSetting;
  };

in
pkgs.vim_configurable.customize {
  name = "vim-coc";
  vimrcConfig = {
    inherit customRC;
    packages.myPlugins.start = with pkgs.vimPlugins; [
      vim-nix # File type and syntax highlighting.
      coc-nvim
      coc-json
      # FIXME
      (nightfox-nvim.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "EdenEast";
          repo = "nightfox.nvim";
          rev = "15f3b5837a8d07f45cbe16753fbf13630bc167a3";
          hash = "sha256-Uq+Rp4uoI+AUEUoSWXInB49bCldPz5f9KtinFMKF8iM=";
        };
      }))
    ];
  };
}
