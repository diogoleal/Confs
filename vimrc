"Diogo Leal - diogo@diogoleal.com

set nocompatible
set background=dark
colorscheme wombat
set guifont=inconsolata\ 12
set guioptions-=T " desabilita barra de ferramentas
set guioptions-=t " desabilita tear-off
set guioptions-=m " desabilita menu
set guioptions-=l
set guioptions-=r
set guioptions-=b
set mouse=a   " habilita uso pleno do mouse
set number
set titlelen=78 " Tamanho da barra de titulo
syntax on
set hlsearch " Ativa o recurso de colorir
set incsearch

" Definindo o zsh
if has("unix")
   let &shell="zsh"
   set clipboard=autoselect
endif   

" Permite remover e adicionar o numeros de linhas
map <C-F11> :set nu!<cr>
imap <C-F11> <Esc>:set nu!<cr>

"Habilita ou desativa o NERDTRee
map <F2> :NERDTree /home/diogo/<cr>
imap <F2> <Esc>:set nu!<cr>

set history=1000 "Exibe 1000 comandos 
set showmode 
set autoindent
set pastetoggle=<F2>
set ruler
set autoread
set sm
set ts=8 " tabstop: numero de colunas para o comando <TAB>
"Tabs sao convertidos para espacos por padrao
set noexpandtab

filetype on       
filetype plugin on
filetype indent on
set report=0

" Usando <BkSpc> para deletar linha
set backspace=eol,start,indent
set softtabstop=3 " Tecla Backspace volta N espacos quando estiver numa indentacao.
set visualbell
set noerrorbells
set autowrite 
set autowriteall
set ttyfast
set bk
set backupdir=~/.vim/.backup/,.
set path=.,./include/,/usr/include/,/usr/local/bin/,~/.vim/scripts/
set title

" Alguns tipos de arquivos devem ser ignorados pelo Vim.
set wildignore=*.o,*.obj,*.bak,*.exe,*.dll,*.com,*.class,*.au,*.wav,*.ps,\
                 \*.avi,*.wmv,*.flv,*.djvu,*.pdf,*.chm,*.dvi,*.svn/,*~

"  Cor da numeracao lateral
"hi LineNr guifg=black ctermfg=black


" URL: http://www.vim.org/scripts/script.php?script_id=2540

"Faz o shift-insert comportar-se semelhante ao Xterm
map  <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>

"Embaralha a tela
map <F4> ggVGg?

" Alterna o modo de quebra de linha
map <leader>b :set wrap! <bar> ec &wrap ? 'wrap' : 'nowrap'<cr>

"set textwidth=120
"   au BufNewFile,BufRead  *  exec 'match Error /\%>' .  &textwidth . 'v.\+/'
" Para remover linhas em branco duplicadas
"  map ,d my:%s/\(^\n\{2,}\)/\r/g`y
   
   iab YDATE <C-R>=strftime("%a %d/%b/%Y hs %H:%M")<CR>
   iab HDATE <C-R>=strftime("%a %d/%b/%Y hs %H:%M")<CR> 
" Identacao de textos e codigos com o TAB no modo visual
" URL: http://gustavodutra.com/post/72/
"               \ dicas-de-movimentacao-e-identacao-no-gvim/
" ----------------------------------------------------------------------------
   vnoremap < <gv
   vnoremap > >gv
   vmap  <TAB> >
   vmap  <S-TAB> <
   imap  <S-TAB> <ESC><<i

" Quebra os arquivos de texto na coluna 79
"  au BufNewFile,BufRead *.txt setl tw=79

" Mostra os espacos em branco inuteis no final da linha 
au BufNewFile,BufRead * syn match brancomala '\s\+$' | hi brancomala ctermbg=red

" MinusculasMaiusculas: converte a primeira letra de cada frase p/MAIUSCULAS
map ,mm :set noic<cr>
       \:%s/\(\(\([.!?]\s*\\|^\s*\)\n^\\|[.?!-] \)\s*"\?\s*\)\([a-zàáéóú]\)/\1\U\4/cg<cr>

" Mapeia <F9> para mostrar/ocultar comentarios
   fu! CommOnOff()
    if !exists('g:hiddcomm')
     let g:hiddcomm=1 | hi Comment ctermfg=black guifg=black
    else
     unlet g:hiddcomm | hi Comment ctermfg=cyan  guifg=cyan term=bold
    endif
   endfu
   map <F9> :call CommOnOff()<cr>

" ----------------------------------------------------------------------------
" Funcao para comentar varios arquivos de acordo com o tipo
" URL: http://vim.wikia.com/wiki/Comment_Lines_according_to_a_given_filetype
" ----------------------------------------------------------------------------
   fu! CommentLines()
     "let Comment="#" " shell, tcl, php, perl
      exe ":s@^@".g:Comment."@g"
      exe ":s@$@".g:EndComment."@g"
   endfu
" mapeando a funcao no modo visual com a combinacao 'co'
   vmap co :call CommentLines()<CR>a
" definindo os comentarios por tipo de arquivo (a primeira linha eh um padrao)
   au BufRead,BufNewFile * let Comment="# " | let EndComment=""
   au BufRead,BufNewFile *.inc,*.ihtml,*.html,*.tpl,*.class 
     \ let Comment="<!-- " | let EndComment=" -->"
   au BufRead,BufNewFile *.sh,*.pl,*.tcl let Comment="# " | let EndComment=""
   au BufRead,BufNewFile *.js set | let Comment="// " | let EndComment=""
   au BufRead,BufNewFile *.cc,*.php,*.cxx,*.cpp 
     \ let Comment="// " | let EndComment=""
   au BufRead,BufNewFile *.c,*.h let Comment="/* " | let EndComment=" */"
   au BufRead,BufNewFile *.f90,*.f95 let Comment="! " | let EndComment=""
   au BufRead,BufNewFile *.f let Comment="C " | let EndComment=""
   au BufRead,BufNewFile *.tex,*.bib let Comment="% " | let EndComment=""
   au BufRead,BufNewFile *.vim,.vimrc let Comment="\" " | let EndComment="" 

 imap ,u <ESC>:source ~/.vimrc<CR> " Permite recarregar e editar o ~/.vimrc

" Para permitir que ele seja automaticamente carregado ao ser salvo
"  autocmd! bufwritepost .vimrc source %
" Para editar o .vimrc
"   imap ,v <ESC>:e ~/.vimrc<CR>

