""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

" set mouse
set mouse=a

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

" Set auto-formating
set formatoptions+=mM
set wrap

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Tab navigation
nnoremap to :tabnew<CR>
nnoremap tc :tabclose<CR>
nnoremap gf <C-W>gf

" Move among windows
noremap <C-h> <C-W>h
noremap <C-j> <C-W>j
noremap <C-k> <C-W>k
noremap <C-l> <C-W>l

" write copy and paste
map <F2> :w<CR>
inoremap <F2> <esc>:w<cr>a
map <F3> "+y
map <F4> o<esc>mpk"+p'pdd
imap <F4> <cr><esc>"+p

" F5 :add separator line
map <F5> o<ESC>78i-<ESC>o<esc>
imap <F5> <cr><esc>78i-<esc>o

" f6 :add time
noremap <F6> :read !date +"[\%Y\%m\%d]"<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" my 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
au FileType c,cpp so ~/.vim/c.vim

" Ctrl - \ inputmethod
let g:vimim_map='c-bslash'
