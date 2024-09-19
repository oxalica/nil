{
  pkgs ? import <nixpkgs> { },
}:
pkgs.vim_configurable.customize {
  name = "vim-lsp";

  vimrcConfig = {
    packages.myVimPackage.start = with pkgs.vimPlugins; [
      vim-nix # File type and syntax highlighting.
      vim-lsp
    ];

    # vim
    customRC = ''
      source ${./vimrc.vim}

      au BufRead,BufNewFile *.nix setf nix

      autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'nil',
        \ 'cmd': {server_info->[$NIL_PATH]},
        \ 'whitelist': ['nix'],
        \ })
      autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()

      function! s:on_lsp_buffer_enabled()
        setlocal omnifunc=lsp#complete
        setlocal tagfunc=lsp#tagfunc

        nmap <buffer> gd <Plug>(lsp-definition)
        nmap <buffer> gr <Plug>(lsp-references)
        nmap <buffer> gs <Plug>(lsp-document-symbol-search)
        nmap <buffer> gS <Plug>(lsp-workspace-symbol-search)
        nmap <buffer> gy <Plug>(lsp-type-definition)
        nmap <buffer> <Space>r <Plug>(lsp-rename)
        nmap <buffer> [g <Plug>(lsp-previous-diagnostic)
        nmap <buffer> ]g <Plug>(lsp-next-diagnostic)
        nmap <buffer> <Space><Space> <Plug>(lsp-hover)
        nnoremap <buffer> <expr><C-f> lsp#scroll(+4)
        nnoremap <buffer> <expr><C-d> lsp#scroll(-4)

        let g:lsp_format_sync_timeout = 1000
        autocmd BufWritePre *.nix call execute('LspDocumentFormatSync')
      endfunction
    '';
  };
}
