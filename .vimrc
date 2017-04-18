
"hi Normal ctermbg=none

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 30
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
map <F2> :NERDTreeToggle<CR>

"Plug 'L9'
Plug 'bronson/vim-trailing-whitespace'

" Go lang
Plug 'fatih/vim-go'
set autowrite

Plug 'tpope/vim-endwise'

Plug 'terryma/vim-multiple-cursors'

" vim simple complete
"Plug 'maxboisvert/vim-simple-complete'
Plug 'Raimondi/delimitMate'

Plug 'w0rp/ale'

set nocompatible
filetype off

let &runtimepath.=',~/.vim/bundle/ale'

filetype plugin on

"winresizer
Plug 'simeji/winresizer'

" git
Plug 'mhinz/vim-signify'
Plug 'ConradIrwin/vim-bracketed-paste'

Plug 'mileszs/ack.vim'
Plug 'wincent/ferret'

Plug 'junegunn/vim-easy-align'

" Easy align interactive
vnoremap <silent> <Enter> :EasyAlign<cr>

"Theme
Plug 'joshdick/onedark.vim'
call plug#end()

syntax enable

set background=dark
colorscheme onedark
filetype indent on
"set t_Co=256
set encoding=utf-8

"set termguicolors
"set cursorline
set laststatus=2
set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L,\ col\ %c)\

set history=700
set number
set ruler
set autoread

"if (exists('+colorcolumn'))
"    set colorcolumn=120
"    highlight ColorColumn ctermbg=1
"endif

set list
set listchars=tab:>-

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,*.git\*,.hg\*,.svn\*

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
    set mouse=a
endif

" Ignore case when searching
set ignorecase

" Highlight search results
set hlsearch
" clean hightlight search
set hlsearch!
nnoremap <F8> :set hlsearch!<CR>

"" Switching windows
"noremap <C-j> <C-w>j
"noremap <C-k> <C-w>k
"noremap <C-l> <C-w>l
"noremap <C-h> <C-w>h

"copy to clipboard
noremap YY "+y<CR>


" Makes search act like search in modern browsers
set incsearch

" For regular expressions turn magic on
set magic

set nobackup
set nowb
set noswapfile

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=2
set tabstop=2

au FileType python setl sw=4 sts=4 et

" Set F3 for using tabs for indentation and F2
" using 4 spaces
noremap <F3> :set noet ci pi sts=0 sw=4 ts=4<CR>
nnoremap <silent> <F2>set shiftwidth=4 tabstop=4 <CR>

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines
"set textwidth=79

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Close the current buffer
map <leader>bd :Bclose<cr>

" Close all the buffers
map <leader>ba :1,1000 bd!<cr>

" Return to last edit position when opening files
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \ exe "normal! g`\"" |
    \ endif

" Toggle paste mode on and off
nnoremap <f5> :set invpaste paste?<CR>
set pastetoggle=<F5>
set showmode

" Returns true if paste mode is enabled
function! HasPaste()
  if &paste
    return 'PASTE MODE  '
  en
  return ''
endfunction
