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
autocmd BufWritePre * let &backupext = strftime(".%Y%m%d_%H%M")

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Interface
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set colorscheme
set background=dark
colorscheme solarized

" 80 column marker
set colorcolumn=80

" Enable syntax highlight
syntax on

"show mode
set showmode

" Show ruler
set ruler

" Dynamic title
function! UpdateTitle()
	let &titlestring = "vim(" . expand("%:t") . ")"
	if &term =~ '^screen'
	  " VimTip #1126
	  " to type ^[, which is an escape character, you need to enter CTRL+V <Esc>
	  set t_ts=k
	  set t_fs=\
	endif
	set title
endfunction
au BufEnter * :call UpdateTitle()
auto VimLeave * :set t_ts=k\

" Hybrid line number
set number
set relativenumber 

" Always have a status line
set laststatus=2

" Allow to display incomplete line
set display=lastline

" cursor line
" set cursorline

" alarm trailing whitespace, except markdown
highlight WhitespaceEOL ctermbg=red guibg=red
match WhitespaceEOL /\s\+$/
autocmd FileType markdown match


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
autocmd BufRead,BufNewFile *.md set filetype=markdown

" Set auto-formating
set formatoptions+=mM

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" change leader key to ,
let mapleader = ","

" move one line
noremap j gj
noremap k gk

" Tab naviation
nnoremap gn :tabnew<CR>
nnoremap gc :tabclose<CR>
" shift+alt+h/l: tab pre/next. generate by press ctrl-v, then ...
nnoremap H :tabprevious<CR>
nnoremap L :tabnext<CR>
" shift+alt+</>: tab move
nnoremap < :tabmove -1<CR>
nnoremap > :tabmove +1<CR>

" Open file under cursor in new tab
nnoremap gf <C-W>gf

" Move among windows
noremap <C-h> <C-W>h
noremap <C-j> <C-W>j
noremap <C-k> <C-W>k
noremap <C-l> <C-W>l

" Line completion
inoremap <c-l> <c-x><c-l>
" Filename completion
inoremap <c-f> <c-x><c-f>

" save
noremap <F2> :w<CR>
inoremap <F2> <esc>:w<cr>a

" Line numbers toggle
nnoremap <silent> <F3> :set number!<CR> :set relativenumber!<CR>

" Paste toggle
set pastetoggle=<F4>

" Save & Make
nnoremap <F5> :w<CR>:make!<CR>
nnoremap <F6> :w<CR>:make! %< CC=gcc CFLAGS="-g -Wall"<CR>:!./%<<CR>

" Ctags, Cscope
nnoremap <F7> :w<CR>:!find $(pwd) -name "*.h" -o -name "*.c" -o -name "*.cc" > cscope.files; cscope -Rbkq -i cscope.files; ctags -R --fields=+lS .<CR><CR>

" Tagbar NERDTree toggle
nnoremap <silent> <F8> :TagbarToggle<CR>:NERDTreeToggle<CR><c-w>l

" Searching tool
nnoremap <F9> :Rgrep<CR>

" ToggleQuickfixList
nmap <script> <silent> <F10> :call ToggleQuickfixList()<CR>

" quickfix, cn cp
nnoremap <silent> <F11> :cprev<CR>
nnoremap <silent> <F12> :cnext<CR>

" Use <space> to toggle fold
nnoremap <silent> <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>

" Insert time
nnoremap <Leader>dt <esc>:read !date +"\%a \%b \%d, \%Y"<cr>I*** <esc>A ***<cr>- - - - - - - - - - - - -<esc>o<esc>

" Open with firefox
nnoremap <Leader>f <esc>:!firefox % &> /dev/null<cr><cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins setting
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set Tagbar width
let tagbar_width = 28
let NERDTreeWinSize = 20

" vim-togglelist Quickfix Open Command
let g:toggle_list_copen_command="botright copen"

" disable folding when using markdown.vim
let g:vim_markdown_folding_disabled=1

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
" Vundle
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
Bundle 'Lokaltog/vim-powerline'

Bundle 'Align'
Bundle 'fcitx.vim'
Bundle 'DrawIt'
Bundle 'The-NERD-Commenter'
Bundle 'https://github.com/milkypostman/vim-togglelist.git'

Bundle 'The-NERD-tree'
Bundle 'Tagbar'
Bundle 'grep.vim'
Bundle 'fugitive.vim'

Bundle 'autoload_cscope.vim'
Bundle 'snipMate'
Bundle 'SuperTab-continued.'
Bundle 'echofunc.vim'
Bundle 'plasticboy/vim-markdown'
filetype plugin indent on

