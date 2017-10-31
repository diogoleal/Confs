
"hi Normal ctermbg=none

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 20
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
map <F2> :NERDTreeToggle<CR>

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
    \ 'Ignored'   : '☒',
    \ "Unknown"   : "?"
    \ }

"Plug 'vim-ctrlspace/vim-ctrlspace'

Plug 'wikitopian/hardmode'

"Plug 'L9'
Plug 'bronson/vim-trailing-whitespace'

"Ruby
Plug 'tpope/vim-endwise'

" lightline
Plug 'itchyny/lightline.vim'
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head'
      \ },
      \ }

" Underlines the word under the cursor
Plug 'itchyny/vim-cursorword'

Plug 'tpope/vim-commentary'

Plug 'itchyny/vim-cursorword'

" Start screen
Plug 'mhinz/vim-startify'

Plug 'terryma/vim-multiple-cursors'
" Default mapping
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_next_key='<ALT-n>'
let g:multi_cursor_prev_key='<ALT-p>'
let g:multi_cursor_skip_key='<ALT-x>'
let g:multi_cursor_quit_key='<Esc>'

" vim simple complete
Plug 'maxboisvert/vim-simple-complete'
"Plug 'Raimondi/delimitMate'

Plug 'w0rp/ale'
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '--'

nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

set nocompatible
set hidden
filetype off
filetype plugin on

let &runtimepath.=',~/.vim/bundle/ale'

"winresizer
Plug 'simeji/winresizer'

Plug 'dag/vim-fish'
" vim-move set to <C-k> and <C-j>
Plug 'matze/vim-move'
let g:move_key_modifier = 'C'

Plug 'kien/ctrlp.vim'
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" Git
Plug 'mhinz/vim-signify'
Plug 'ConradIrwin/vim-bracketed-paste'
Plug 'jreybert/vimagit'
Plug 'tpope/vim-fugitive'

Plug 'mileszs/ack.vim'
Plug 'wincent/ferret'

Plug 'junegunn/vim-easy-align'

"Ansible
Plug 'pearofducks/ansible-vim'

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

set termguicolors
set cursorline
set laststatus=2

set history=700
set number
set ruler
set autoread
set autowrite

" Enabled color column
if (exists('+colorcolumn'))
    set colorcolumn=120
    highlight ColorColumn ctermbg=1
endif

set list
set listchars=tab:>-
set showbreak=>>>

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,*.git\*,.hg\*,.svn\*,*.lock

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

" Set F3 for using tabs for indentation and F4
" using 4 spaces
noremap <F3> :set noet ci pi sts=0 sw=4 ts=4<CR>
nnoremap <silent> <F4>set shiftwidth=4 tabstop=4 <CR>

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines
set textwidth=119

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

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
