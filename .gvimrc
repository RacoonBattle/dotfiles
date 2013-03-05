"gvim---------------------------------------------------
"fonts set

set guifont=DejaVu\ Sans\ Mono\ 12
set guifontwide=AR\ PL\ UMing\ TW\ 13

"Toggle Menu and Toolbar
set guioptions-=T
set guioptions-=m

"color scheme
colorscheme desert

"window position
winpos 0 0 

"window size
set lines=55 columns=88

"line space
set linespace=3

"Change work dir to current dir
autocmd BufEnter * cd %:p:h
