"gvim---------------------------------------------------
"fonts set
set guifont=DejaVu\ Sans\ Mono\ 12
set guifontwide=AR\ PL\ UMing\ TW\ 13

"color scheme
colorscheme morning

"line space
set linespace=3

"Change work dir to current dir
autocmd BufEnter * cd %:p:h

" Cut, copy and paste
vnoremap <S-Del> "+x
vnoremap <C-Insert> "+y
imap <S-Insert> <MiddleMouse>
cmap <S-Insert> <MiddleMouse>
