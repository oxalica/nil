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

    set updatetime=300
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
  '';

  cocSetting = {
    languageserver.nix = {
      command = "nil";
      filetypes = [ "nix" ];
      rootPatterns =  [ "flake.nix" ];
    };
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
    ];
  };
}
