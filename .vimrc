set nocompatible

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>

Plug 'Xuyuanp/nerdtree-git-plugin'
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "✹",
    \ "Staged"    : "✚",
    \ "Untracked" : "✭",
    \ "Renamed"   : "➜",
    \ "Unmerged"  : "═",
    \ "Deleted"   : "✖",
    \ "Dirty"     : "✗",
    \ "Clean"     : "✔︎",
    \ "Unknown"   : "?"
    \ }

Plug 'ctrlpvim/ctrlp.vim'
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

Plug 'L9'
Plug 'bronson/vim-trailing-whitespace'
Plug 'Yggdroot/indentLine'

Plug 'majutsushi/tagbar'
nmap <silent> <F4> :TagbarToggle<CR>
let g:tagbar_autofocus = 1

let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [  'p:package', 'i:imports:1', 'c:constants', 'v:variables',
        \ 't:types',  'n:interfaces', 'w:fields', 'e:embedded', 'm:methods',
        \ 'r:constructor', 'f:functions' ],
    \ 'sro' : '.',
    \ 'kind2scope' : { 't' : 'ctype', 'n' : 'ntype' },
    \ 'scope2kind' : { 'ctype' : 't', 'ntype' : 'n' },
    \ 'ctagsbin'  : 'gotags',
    \ 'ctagsargs' : '-sort -silent'}

" c
Plug 'vim-scripts/c.vim'
Plug 'Shougo/vimproc.vim'

" Shell
Plug 'xolox/vim-shell'
Plug 'xolox/vim-misc'

" git
Plug 'mhinz/vim-signify'
Plug 'ConradIrwin/vim-bracketed-paste'
Plug 'scrooloose/syntastic'
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"let g:syntastic_python_checkers=['pyflakes']
"let g:syntastic_python_checkers=['python', 'flake8']
"let g:syntastic_python_flake8_post_args='--ignore=W391'

" Language pack
Plug 'sheerun/vim-polyglot'

" Simplenote
"Plug 'mrtazz/simplenote.vim'
"source ~/.simplenoterc

Plug 'NLKNguyen/papercolor-theme'

call plug#end()

syntax enable
filetype plugin indent on
set t_Co=256
set background=dark
colorscheme PaperColor

set history=700
set number
set ruler
set autoread

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
set shiftwidth=4
set tabstop=4

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

"" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Close the current buffer
map <leader>bd :Bclose<cr>

" Close all the buffers
map <leader>ba :1,1000 bd!<cr>

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \ exe "normal! g`\"" |
    \ endif

" Always show the status line
"set laststatus=2

" Format the status line
"set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c

" Status line {{{1
"set statusline=
"set statusline+=%1*\ [%2*%2n%1*]  " Buffer number
"set statusline+=%<  " Truncate the path if needed
"set statusline+=%3*\ %f  " File name
"set statusline+=%4*%5r  " ReadOnly flag
"set statusline+=%5*\ %y  " File type
"set statusline+=%6*\ %m  " Modified flag

"set statusline+=%=  " Separation

"set statusline+=%1*\ [col\ %3*%v%1*]  " Virtual column number
"set statusline+=%1*\ [row\ %2*%l%1*/%2*%L%1*\ %p%%]  " Current/total line
"set statusline+=%1*\ [byte\ %5*%o%1*]  " Byte number in file

"hi User1 ctermfg=255 guifg=#eeeeee ctermbg=235 guibg=#262626
"hi User2 ctermfg=167 guifg=#d75757 ctermbg=235 guibg=#262626
"hi User3 ctermfg=107 guifg=#87af5f ctermbg=235 guibg=#262626
"hi User4 ctermfg=33 guifg=#0087ff ctermbg=235 guibg=#262626
"hi User5 ctermfg=221 guifg=#ffd75f ctermbg=235 guibg=#262626
"hi User6 ctermfg=133 guifg=#af5faf ctermbg=235 guibg=#262626

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

