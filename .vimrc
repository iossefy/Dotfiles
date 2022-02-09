set nocompatible
set encoding=utf-8
set background=dark

syntax on
filetype on                     " Enable file type detection
filetype indent on              " Enable file type-specific indenting
filetype plugin on              " Enable file type-specific plugins
filetype plugin on
filetype plugin indent on

silent! set cryptmethod=blowfish2

" set viminfo+=n~/.vim/viminfo
" set viminfo='50,<1000,s100,:0,n~/vim/viminfo

set textwidth=80                " Set text width to 80
set numberwidth=4               " Set number width to 4
set incsearch                   " Find the next match as we type the search
set hlsearch                    " Highlight searches by default
set ignorecase                  " Ignore case when searching...
set smartcase                   " ...unless we type a capital
set softtabstop=4
set formatoptions+=r
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4

set copyindent                  " copy the previous indentation on auto indenting
set hidden
set nowrap
set fileformats=unix,dos,mac   " support all three, in this order

set foldmethod=manual
set foldlevel=7
set tags=tags;
set clipboard=unnamedplus
set expandtab
set smarttab
set backspace=indent,eol,start
set magic                       " Does some magic ;-) Newline characters...
set ttyfast                     " The current terminal is a fast terminal so

set laststatus=2
set history=72		            " keep 72 lines of command line history
set undofile                    " Save undo(s) after file closes
set undodir=$HOME/.vim/undo     " where to save undo histories
set undolevels=1000             " How many undo(s)
set undoreload=10000            " number of lines to save for undo
set directory=~/.vim/swap/

set nospell

set wildignore=*.o,*~,*.pyc,*.so " ignore compiled files

set noerrorbells		         " No annoying sound on errors
set novisualbell

set ls=2
set nu
set ruler                       " show the cursor position all the time
set title
set showcmd                     " display incomplete commands

set autowrite                   " Automatically :w before running commands
set showmode                    " show current mode down the bottom
set autoread
set wildmode=longest,list,full
set wildmenu
set t_Co=256
set clipboard=

"set cursorline

" close braces automatically
"inoremap " ""<left>
"inoremap ' ''<left>
"inoremap ( ()<left>
"inoremap [ []<left>
"inoremap { {}<left>
"inoremap {<CR> {<CR>}<ESC>O
"inoremap {;<CR> {<CR>};<ESC>O

" ctrl+arrows navigate tabs
map <C-left> :tabp<cr>
map <C-right> :tabn<cr>

" By pressing ctrl+r in the visual mode you will be prompted to enter text to replace with.
vnoremap <C-r> "hy:%s/<C-r>h//g<left><left>

" Automatically deletes all tralling whitespace on save.
autocmd BufWritePre * %s/\s\+$//e
autocmd FileType json syntax match Comment +\/\/.\+$+

" Clear highlighting on escape in normal mode
nnoremap <esc> :noh<return><esc>
nnoremap <esc>^[ <esc>^[

" Easier split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" More natural split opening
set splitbelow
set splitright

nmap <leader>ne :NERDTreeToggle<cr>
nmap <leader>dt :ALEDetail<cr>

let python_highlight_all = 1

au FileType python syn keyword pythonDecorator True None False self

au BufNewFile,BufRead *.jinja set syntax=htmljinja
au BufNewFile,BufRead *.mako set ft=mako

au FileType python map <buffer> F :set foldmethod=indent<cr>

au FileType python inoremap <buffer> $r return
au FileType python inoremap <buffer> $i import
au FileType python inoremap <buffer> $p print
au FileType python inoremap <buffer> $f # --- <esc>a

au FileType python map <buffer> <leader>1 /class
au FileType python map <buffer> <leader>2 /def
au FileType python map <buffer> <leader>C ?class
au FileType python map <buffer> <leader>D ?def



" Here is my plugins
call plug#begin()
Plug 'RRethy/vim-illuminate'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'vim-airline/vim-airline-themes'
Plug 'dense-analysis/ale'
"Plug 'neoclide/coc.nvim'

Plug 'vim-airline/vim-airline'
Plug 'preservim/nerdcommenter'

Plug 'vim-airline/vim-airline-themes'
Plug 'airblade/vim-gitgutter'


" Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

highlight pythonSpaceError ctermfg=0
highlight Normal ctermfg=white
highlight Comment ctermfg=green

"  colorscheme gruvbox
colorscheme wal

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_section_z = '%3p%% %#__accent_bold#%{g:airline_symbols.linenr}%4l%#__restore__#%#__accent_bold#/%L%#__restore__# :%3v'
let g:NERDCustomDelimiters = { 'c': { 'left': '/* ','right': ' */' } }
"let g:cpp_no_function_highlight = 1
let c_no_curly_error=1


" air-line
"let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
"let g:airline_left_sep = '»'
"let g:airline_left_sep = '▶'
"let g:airline_right_sep = '«'
"let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
"let g:airline_left_sep = ''
"let g:airline_left_alt_sep = ''
"let g:airline_right_sep = ''
"let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''


" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1


" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif

