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
if &term == "screen-256color"
	set title
	set t_ts=k
	set t_fs=\
endif
if &term == "xterm-256color"
	set title
endif

" Display line number
"set number
set relativenumber

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

filetype plugin indent on
"for python
autocmd FileType python setlocal expandtab smarttab shiftwidth=4 softtabstop=4
"for wiki
autocmd FileType flexwiki setlocal noexpandtab smarttab shiftwidth=4 softtabstop=4
"for mail
autocmd FileType mail set textwidth=72

" Set auto-formating
set formatoptions+=mM
set wrap

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" move one line
noremap j gj
noremap k gk

" Tab naviation
"nnoremap L :tabnext<CR>
"nnoremap H :tabprevious<CR>
"nnoremap tn :tabnew<CR>
"nnoremap tc :tabclose<CR>

nnoremap gf <C-W>gf

" Textwidth=78
noremap tw :set textwidth=78

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

" write copy and paste
noremap <F2> :w<CR>
inoremap <F2> <esc>:w<cr>a
noremap <F3> "+y
"noremap <F4> o<esc>mpk"+p'pdd
"inoremap <F4> <cr><esc>"+p
set pastetoggle=<F4>

" Grep search tools
nnoremap <F6> :Rgrep<CR>

" Save & Make
nnoremap <F7> :w<CR>:make! %< CC=gcc CFLAGS="-g -Wall"<CR>:!./%<<CR>
inoremap <F7> <ESC>:w<CR>:make! %< CC=gcc CFLAGS="-g -Wall"<CR>:!./%<<CR>

" Quickfix window
"nnoremap <silent> <F7> :botright copen<CR>
"nnoremap <silent> <F8> :cclose<CR>
nnoremap <F8> :call ToggleList("Quickfix List", 'c')<CR>
inoremap <F8> <ESC>:call ToggleList("Quickfix List", 'c')<CR>

" Toggle Tagbar
nnoremap <silent> <F9> :TagbarToggle<CR>
inoremap <silent> <F9> <ESC>:TagbarToggle<CR>

" NERDTreeToggle
nnoremap <silent> <F10> :NERDTreeToggle<CR>
inoremap <silent> <F10> <ESC>:NERDTreeToggle<CR>

" F11 :add separator line
noremap <F11> o<ESC>78i-<ESC>o<esc>
inoremap <F11> <cr><esc>78i-<esc>o

" F12 :add time
noremap <F12> :read !date +"[\%Y\%m\%d]"<cr>
inoremap <F12> <ESC>:read !date +"[\%Y\%m\%d]"<cr>o

" Cscope mappings
nmap <C-\>s :scs find s <C-R>=expand("<cword>")<CR><CR>	
nmap <C-\>g :scs find g <C-R>=expand("<cword>")<CR><CR>	
nmap <C-\>c :scs find c <C-R>=expand("<cword>")<CR><CR>	
nmap <C-\>t :scs find t <C-R>=expand("<cword>")<CR><CR>	
nmap <C-\>e :scs find e <C-R>=expand("<cword>")<CR><CR>	
nmap <C-\>f :scs find f <C-R>=expand("<cfile>")<CR><CR>	
nmap <C-\>i :scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-\>d :scs find d <C-R>=expand("<cword>")<CR><CR>	

" 

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vundle
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" git clone http://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'Lokaltog/vim-powerline'
Bundle "myusuf3/numbers.vim"
Bundle 'Tagbar'
"Bundle 'taglist.vim'
Bundle 'fcitx.vim'
Bundle 'The-NERD-tree'
Bundle 'snipMate'
Bundle 'SuperTab-continued.'
Bundle 'grep.vim'
"Bundle 'xptemplate'
"Bundle 'autoload_cscope.vim'
"Bundle 'DrawIt'
"Bundle 'echofunc.vim'
filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins setting
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" au FileType c,cpp so ~/.vim/c.vim

" Ctrl - \ inputmethod
let g:vimim_map='c-bslash'

" Set Tagbar width
let tagbar_width = 25

" Super tab completion type
let g:SuperTabDefaultCompletionType = "context"

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

"nmap <silent> <leader>l :call ToggleList("Location List", 'l')<CR>
"nmap <silent> <leader>e :call ToggleList("Quickfix List", 'c')<CR>
