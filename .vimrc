vim9script
set nocompatible
set encoding=utf-8
set background=dark

syntax on
filetype on                      # Enable file type detection
filetype indent on               # Enable file type-specific indenting
filetype plugin on               # Enable file type-specific plugins
filetype plugin on
filetype plugin indent on

set textwidth=80                 # Set text width to 80
set relativenumber
set numberwidth=4                # Set number width to 4
set incsearch                    # Find the next match as we type the search
set hlsearch                     # Highlight searches by default
set ignorecase                   # Ignore case when searching...
set smartcase                    # ...unless we type a capital
set softtabstop=4
set formatoptions+=r
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4

set copyindent                   # copy the previous indentation on auto indenting
set hidden
set nowrap
set fileformats=unix,dos,mac     # support all three, in this order

set foldmethod=manual
set foldlevel=7
set tags=tags;
set clipboard=unnamedplus
set expandtab
set smarttab
set backspace=indent,eol,start
set magic                        # Does some magic ;-) Newline characters...
set ttyfast                      # The current terminal is a fast terminal so

set laststatus=2
set history=72                   # keep 72 lines of command line history
set undofile                     # Save undo(s) after file closes
set undodir=$HOME/.vim/undo      # where to save undo histories
set undolevels=1000              # How many undo(s)
set undoreload=10000             # number of lines to save for undo
set directory=~/.vim/swap/

set nospell

set wildignore=*.o,*~,*.pyc,*.so # ignore compiled files

set noerrorbells                 # No annoying sound on errors
set novisualbell

set ls=2
set nu
set ruler                        # show the cursor position all the time
set title
set showcmd                      # display incomplete commands

set autowrite                    # Automatically :w before running commands
set showmode                     # show current mode down the bottom
set autoread
set wildmode=longest,list,full
set wildmenu
set t_Co=256
set clipboard=

# ctrl+arrows navigate tabs
map <C-left> :tabp<cr>
map <C-right> :tabn<cr>

# Easier split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

# More natural split opening
set splitbelow
set splitright

nmap <leader>ne :NERDTreeToggle<cr>
nmap <leader>dt :ALEDetail<cr>

# Here is my plugins
call plug#begin()
Plug 'preservim/nerdcommenter'
call plug#end()

colorscheme wal

# when not using colorscheme
#highlight pythonSpaceError ctermfg=0
#highlight Normal ctermfg=white
#highlight Comment ctermfg=green
