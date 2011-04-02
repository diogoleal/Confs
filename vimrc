 set nocompatible
" Definindo o zsh
   if has("unix")
    let &shell="zsh"
    set clipboard=autoselect
   endif   
" Numero de linhas"
   set number

" Permite remover e adicionar o numeros de linhas
   map <C-F11> :set nu!<cr>
   imap <C-F11> <Esc>:set nu!<cr>


" Habilita ou desativa o NERDTRee
   map <C-F2> :NERDTree /home/diogo/<cr>
"   imap <C-F2> <Esc>:set nu!<cr>

set history=1000 "Exibe 1000 comandos 
set showmode 
set autoindent
set pastetoggle=<F2>
set ruler
set autoread
set sm
set ts=8 " tabstop: numero de colunas para o comando <TAB>
" Tabs são convertidos para espaços por padrão
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
set titlelen=78 " Tamanho da barra de titulo
   syntax on

"  set background=light
"   map  <S-F6> <ESC>:set background=light<CR>
"  map  <C-S-F6> <ESC>:set background=dark<CR>

   colorscheme moria
   function! <SID>SwitchColorSchemes()
    if g:colors_name == 'moria'
     colorscheme automation
    elseif g:colors_name == 'automation'
     colorscheme moria
    elseif g:colors_name == 'moria'
     colorscheme desert
    elseif g:colors_name == 'desert'
     colorscheme colorful
    elseif g:colors_name == 'colorful'
     colorscheme navajo-night
    elseif g:colors_name == 'navajo-night'
     colorscheme bmichaelsen
    elseif g:colors_name == 'bmichaelsen'
     colorscheme impact
    elseif g:colors_name == 'impact'
     colorscheme ir_black
    elseif g:colors_name == 'ir_black'
      set background=dark
     colorscheme native
     endif
    endfunction
    map <F6> :call <SID>SwitchColorSchemes()<CR>:echo g:colors_name<CR>

    "  Cor da numeração lateral 
hi LineNr     guifg=green    ctermfg=lightGreen 

   "  " Configuração de fonte (tamanho e nome) para o GVim
   if has("gui_running")
	  if has("gui_gtk2")
       if exists("$SCREENSIZE")
        if ($SCREENSIZE > 1300)
          set guifont=monospace\ 14
        elseif ($SCREENSIZE < 850)
          set guifont=monospace\ 8
        else
          set guifont=monospace\ 10
        endif 
       else
 		  set guifont=monospace\ 10
       endif  
	  elseif has("x11")
		 set guifont=*-lucidatypewriter-medium-r-normal-*-*-180-*-*-m-*-*
     elseif has("gui_win32")
		 set guifont=Luxi_Mono:h12:cANSI
     else
		 set guifont=monospace\ 12
     endif 
   endif 

   set hlsearch " Ativa o recurso de colorir
   set incsearch

" ----------------------------------------------------------------------------
" URL: http://www.vim.org/scripts/script.php?script_id=2540
" ----------------------------------------------------------------------------
" Os snippets utilizados por esse .vimrc e que são diferentes da versão de
" instalação, podem ser encontrados em:
" URL: http://snipt.net/jfmitre/tag/snippet
" ----------------------------------------------------------------------------
" O snipMate deve usar tabulação para funcionar
" ----------------------------------------------------------------------------
"    au BufNewFile,BufRead *.snippets set noexpandtab
" ----------------------------------------------------------------------------
" Não quero usar folder nos arquivos do snipMate
" ----------------------------------------------------------------------------
"   au BufNewFile,BufRead *.snippets  set foldlevel=2
" ----------------------------------------------------------------------------

" Faz o shift-insert comportar-se semelhante ao Xterm
   map  <S-Insert> <MiddleMouse>
   map! <S-Insert> <MiddleMouse>
" ----------------------------------------------------------------------------
" Atalhos para o plugin Calendar
" ----------------------------------------------------------------------------
" URL: http://www.vim.org/scripts/script.php?script_id=52
" ----------------------------------------------------------------------------  
"   cab C Calendar
"   cab CH CalendarH
" ----------------------------------------------------------------------------
" Embaralha a tela para evitar bisbilhoteiros
" ----------------------------------------------------------------------------
   map <F4> ggVGg?
" ----------------------------------------------------------------------------
" Alterna o modo de quebra de linha
" ----------------------------------------------------------------------------
   map <leader>b :set wrap! <bar> ec &wrap ? 'wrap' : 'nowrap'<cr>
" ----------------------------------------------------------------------------

"set textwidth=120
"   au BufNewFile,BufRead  *  exec 'match Error /\%>' .  &textwidth . 'v.\+/'
" Para remover linhas em branco duplicadas
"  map ,d my:%s/\(^\n\{2,}\)/\r/g`y
   
" Dando destaque para notas
" http://vivaotux.blogspot.com/2009/01/uniformizao-de-espaamento-nos-cdigos.html
   highlight MinhasNotas ctermbg=blue ctermfg=yellow guibg=blue guifg=yellow
   match MinhasNotas /[Nn]otas\? \?:/  
" ----------------------------------------------------------------------------
" a função (strftime) é predefinida pelo sistema
" ----------------------------------------------------------------------------
   iab YDATE <C-R>=strftime("%a %d/%b/%Y hs %H:%M")<CR>
   iab HDATE <C-R>=strftime("%a %d/%b/%Y hs %H:%M")<CR> 
" Identação de textos e códigos com o TAB no modo visual
" URL: http://gustavodutra.com/post/72/
"               \ dicas-de-movimentacao-e-identacao-no-gvim/
" ----------------------------------------------------------------------------
   vnoremap < <gv
   vnoremap > >gv
   vmap  <TAB> >
   vmap  <S-TAB> <
   imap  <S-TAB> <ESC><<i
" ----------------------------------------------------------------------------
" Permite abrir um arquivo mencionado com o caminho dentro de outro arquivo
" Funciona em cabeçalhos de programação
" ----------------------------------------------------------------------------
   nmap gf :new %:p:h/<cfile><CR>
" ----------------------------------------------------------------------------

" Quebra os arquivos de texto na coluna 79
"  au BufNewFile,BufRead *.txt setl tw=79

" Mapeia <F9> para mostrar/ocultar comentários
   fu! CommOnOff()
    if !exists('g:hiddcomm')
     let g:hiddcomm=1 | hi Comment ctermfg=black guifg=black
    else
     unlet g:hiddcomm | hi Comment ctermfg=cyan  guifg=cyan term=bold
    endif
   endfu
   map <F9> :call CommOnOff()<cr>
" ----------------------------------------------------------------------------
" Função para comentar vários arquivos de acordo com o tipo
" URL: http://vim.wikia.com/wiki/Comment_Lines_according_to_a_given_filetype
" ----------------------------------------------------------------------------
   fu! CommentLines()
     "let Comment="#" " shell, tcl, php, perl
      exe ":s@^@".g:Comment."@g"
      exe ":s@$@".g:EndComment."@g"
   endfu
" mapeando a função no modo visual com a combinação 'co'
   vmap co :call CommentLines()<CR>a
" definindo os comentários por tipo de arquivo (a primeira linha é um padrão)
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
   imap ,v <ESC>:e ~/.vimrc<CR>
if has("gui_running")
   set guioptions-=T " desabilita barra de ferramentas
   set guioptions-=t " desabilita tear-off
   set guioptions-=m " desabilita menu
   set guioptions-=l
   set guioptions-=r
   set guioptions-=b

   set mouse=a   " habilita uso pleno do mouse
   colorscheme synic
   "set gfn=Monaco
endif
