{ pkgs ? import <nixpkgs> { } }:
let
  # vim
  customRC = ''
    source ${./vimrc.vim}

    " Dunno why, but ftdetect seems not working.
    au BufRead,BufNewFile *.nix setf nix

    let g:coc_node_path = '${pkgs.nodejs}/bin/node'
    let g:coc_config_home = '${cocConfigHome}'
    let g:coc_data_home = (empty($TMPDIR) ? '/tmp' : $TMPDIR) . '/coc-data'
    let $PATH = '${wrapper}/bin:' . $PATH
    let leader = '\\'

    set updatetime=300
    syntax off " Disable the builtin regex-based highlighting for semantic tokens.

    inoremap <silent><expr> <TAB> coc#pum#visible() ? coc#pum#confirm() : "\<Tab>"
    inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
    inoremap <silent><expr> <c-@> coc#refresh()

    nmap <silent> [d <Plug>(coc-diagnostic-prev)
    nmap <silent> ]d <Plug>(coc-diagnostic-next)
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    nmap <silent> <C-s> <Plug>(coc-range-select)
    xmap <silent> <C-s> <Plug>(coc-range-select)

    nnoremap <silent> <Space><Space> <Cmd>call CocActionAsync('doHover')<CR>
    nnoremap <silent> <Space>s       <Cmd>call CocActionAsync('showSignatureHelp')<CR>
    nmap     <silent> <Space>a       <Plug>(coc-codeaction)
    nmap     <silent> <Space>r       <Plug>(coc-rename)

    command -nargs=0 CocShowOutput CocCommand workspace.showOutput languageserver.nix
    command -nargs=0 CocSemanticHighlightInfo call CocActionAsync('showSemanticHighlightInfo')

    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
    packadd! nightfox.nvim
    colorscheme nightfox

    highlight link Identifier        TSVariable
    highlight link CocSemPath        Include
    highlight link CocSemVariable    TSVariable
    highlight link CocSemParameter   TSVariable
    highlight link CocSemPunctuation TSOperator

    highlight link CocSemEscape     TSStringEscape
    highlight link CocSemUnresolved Error

    highlight link CocSemDelimiterPunctuation   TSPunctDelimiter
    highlight link CocSemConditionalKeyword     Conditional
    highlight link CocSemDefaultLibraryVariable TSConstBuiltin
    highlight link CocSemDefaultLibraryFunction TSFuncBuiltin
    highlight link CocSemDefaultLibrary         TSVariableBuiltin
  '';

  cocSetting = {
    "coc.preferences.formatOnSaveFiletypes" = [ "nix" ];
    languageserver.nix = {
      command = "nil";
      filetypes = [ "nix" ];
      rootPatterns =  [ "flake.nix" ];
      settings.nil = {
        testSetting = 42;
        formatting.command = [ "nixpkgs-fmt" ];
      };
    };
    semanticTokens.filetypes = [ "nix" ];
  };

  wrapper = pkgs.writeShellScriptBin "nil" ''
    exec "$NIL_PATH" "$@"
  '';

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
      coc-nvim
      nightfox-nvim
    ];
  };
}
