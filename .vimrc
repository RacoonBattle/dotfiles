""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" auto reload vimrc
autocmd bufwritepost .vimrc source $MYVIMRC

" Be IMproved
set nocompatible

" file encodings
set fileencodings=utf-8,cp936,big5,gb2312,gbk,gb18030
" internal encoding
set encoding=utf-8

" Set wild menu & mode
set wildmenu

" set wild mode
set wildmode=longest:full,full

" Auto change current directory
set autochdir

" Lines folding
set foldenable
set foldnestmax=1
set foldmethod=syntax

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Backup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable backup
set backup

" Set backup directory
set backupdir=~/.vim/backup

" Set swap file directory
set directory=~/.vim/swap,/tmp

" Keep more backups for one file
autocmd BufWritePre * let &backupext = strftime(".%m-%d-%H-%M")

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Interface
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set colorscheme
colorscheme elflord

" Enable syntax highlight
syntax on

"show mode
set showmode

" Show ruler
set ruler

" Dynamic title
set title

" Display line number
set number

" Always have a status line
set laststatus=2

" Allow to display incomplete line
set display=lastline

" cursor line
set cursorline

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Highlight search things
set hlsearch

" Ignore case when searching
set smartcase
set ignorecase

" Incremental match when searching
set incsearch

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Indent
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Smart indet
set smartindent

" tab
set tabstop=8
set softtabstop=8
set shiftwidth=8
set noexpandtab

" Config C indent as kernel codingstyle
set cinoptions=:0,l1,t0,g0

filetype plugin indent on
"for python
autocmd FileType python setlocal expandtab smarttab shiftwidth=4 softtabstop=4
"for wiki
autocmd FileType flexwiki setlocal noexpandtab smarttab shiftwidth=4 softtabstop=4
"for mail
autocmd FileType mail set textwidth=72
"for markdown
autocmd BufRead,BufNewFile *.{md,mdown,mkd,mkdn,markdown,mdwn} set filetype=mkd

" Set auto-formating
set formatoptions+=mM

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" move one line
noremap j gj
noremap k gk

" Tab naviation
nnoremap gn :tabnew<CR>
nnoremap gc :tabclose<CR>
" shift+alt+h/l, tab pre/next. generate by press ctrl-v, then ...
nnoremap H :tabprevious<CR>	
nnoremap L :tabnext<CR>

" Open file under cursor in new tab
nnoremap gf <C-W>gf

" Move among windows
noremap <C-h> <C-W>h
noremap <C-j> <C-W>j
noremap <C-k> <C-W>k
noremap <C-l> <C-W>l
noremap <C-W> <C-W><C-W>

" Line completion
inoremap <c-l> <c-x><c-l>
" Filename completion
inoremap <c-f> <c-x><c-f>

" save
noremap <F2> :w<CR>
inoremap <F2> <esc>:w<cr>a

" F3 and F4: copy and paste from 'the selection buffer'
vmap <F3> "*y
noremap <F4> "*p
inoremap <F4> <ESC>"*p

" SHIFT + F3 and F4: copy and paste from 'the system clipboard'
" note <S-F3> need type by ctrl+v and shift+f3, same to <S-F4>
vmap [1;2R "+y
noremap [1;2S "+p
inoremap [1;2S <ESC>"+p

"insert [time]
nmap <F5> :read !date +"\%a \%b \%d, \%Y"<cr>o- - -<esc>o<esc>
imap <F5> <ESC><F5>a<tab>

" NERDTreeToggle
nnoremap <silent> <F6> :NERDTreeToggle<CR>

" Quickfix window
nnoremap <F7> :call ToggleList("Quickfix List", 'c')<CR>

" Toggle Tagbar
nnoremap <silent> <F8> :TagbarToggle<CR>

" Grep search tools
nnoremap <F9> :Rgrep<CR>

" Save & Make, F10: gcc; ctrl+f10:tags; shift+f10: make
nnoremap <F10> :w<CR>:make! %< CC=gcc CFLAGS="-g -Wall"<CR>:!./%<<CR>
nnoremap [21;5~ :w<CR>:!find $(pwd) -name "*.h" -o -name "*.c" -o -name "*.cc" > cscope.files; cscope -Rbkq -i cscope.files; ctags -R --fields=+lS .<CR><CR>
nnoremap [21;2~ :w<CR>:make!<CR>

" quickfix, cn cp
nnoremap <silent> <F11> :cprev<CR>
nnoremap <silent> <F12> :cnext<CR>

" Use <space> to toggle fold
nnoremap <silent> <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins setting
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" change leader key to ,
let mapleader = ","
" vim-EasyMotion_leader_key 
let g:EasyMotion_leader_key = '<Leader>'

" Set Tagbar width
let tagbar_width = 25

" Super tab completion type
let g:SuperTabDefaultCompletionType = "context"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ctags & Cscope
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Auto finding
set tags=tags;

" Use both cscope and ctag
set cscopetag

" Show msg when cscope db added
set cscopeverbose

" Use tags for definition search first
set cscopetagorder=1

" Use quickfix window to show cscope results
set cscopequickfix=s-,g-,d-,c-,t-,e-,f-,i-

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Toggle Quickfix function
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! GetBufferList()
  redir =>buflist
  silent! ls
  redir END
  return buflist
endfunction

function! ToggleList(bufname, pfx)
  let buflist = GetBufferList()
  for bufnum in map(filter(split(buflist, '\n'), 'v:val =~ "'.a:bufname.'"'), 'str2nr(matchstr(v:val, "\\d\\+"))')
    if bufwinnr(bufnum) != -1
      exec(a:pfx.'close')
      return
    endif
  endfor
  if a:pfx == 'l' && len(getloclist(0)) == 0
      echohl ErrorMsg
      echo "Location List is Empty."
      return
  endif
  let winnr = winnr()
  exec(a:pfx.'open')
  if winnr() != winnr
    wincmd p
  endif
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vundle
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" git clone http://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
Bundle 'Lokaltog/vim-powerline'
Bundle 'Lokaltog/vim-easymotion'

Bundle 'Align'
Bundle 'fcitx.vim'
Bundle 'DrawIt'
Bundle 'The-NERD-Commenter'

Bundle 'The-NERD-tree'
Bundle 'Tagbar'
Bundle 'grep.vim'

Bundle 'autoload_cscope.vim'
Bundle 'snipMate'
Bundle 'SuperTab-continued.'
Bundle 'echofunc.vim'
filetype plugin indent on

