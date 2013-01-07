"Diogo Leal - diogo@diogoleal.com

set nocompatible
"set background=light
colorscheme xemacs
set guifont=inconsolata\ 13
  
set guioptions-=T " desabilita barra de ferramentas
set guioptions-=t " desabilita tear-off
set guioptions-=m " desabilita menu
set guioptions-=l
set guioptions-=r
set guioptions-=b
set mouse=a   " habilita uso pleno do mouse
set number
syntax on
set hlsearch " Ativa o recurso de colorir

set history=50
set showmode 
set autoindent
set pastetoggle=<F2>
set ruler
set autoread
set sm

"Indentation 
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab

set nobackup
set noswapfile

set backspace=eol,start,indent
"set visualbell
set noerrorbells
set autowrite 
set autowriteall
set ttyfast

"Embaralha a tela
map <F4> ggVGg?

" Mostra os espacos em branco inuteis no final da linha 
au BufNewFile,BufRead * syn match brancomala '\s\+$' | hi brancomala ctermbg=red

" Mapeia <F9> para mostrar/ocultar comentarios
   fu! CommOnOff()
    if !exists('g:hiddcomm')
     let g:hiddcomm=1 | hi Comment ctermfg=black guifg=black
    else
     unlet g:hiddcomm | hi Comment ctermfg=cyan  guifg=cyan term=bold
    endif
   endfu
   map <F9> :call CommOnOff()<cr>
