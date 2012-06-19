	
inoremap ( ()<ESC>i
inoremap ) <c-r>=ClosePair(')')<CR>
inoremap { {<CR>}<esc>O
inoremap } <c-r>=ClosePair('}')<CR>
map <C-G> :!gcc -Wall -g %<CR>
imap <C-G> <esc>:!gcc -Wall -g%<CR>
"map <F3> :!gcc -Wall % -o %:r.out<CR>
"imap <F3> <esc>:!gcc -Wall % -o %:r.out<CR>

":inoremap [ []<ESC>i
":inoremap ] <c-r>=ClosePair(']')<CR>
":inoremap " ""<ESC>i
":inoremap ' ''<ESC>i

function! ClosePair(char)
 if getline('.')[col('.') - 1] == a:char
     return "\<Right>"
 else
     return a:char
 endif
endfunction

