ZSHA_BASE=$HOME/.zsh-antigen
source $ZSHA_BASE/antigen.zsh

export EDITOR='vim'

export PANEL_FIFO="/tmp/panel-fifo"
export PATH=/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin:/bin:~/bin

# vim-manpager
export MANPAGER="vim -c MANPAGER -"

LS_COLORS='*.png=96:*.aux=90:*.bib=94:*.log=1;90:*.pdf=1;93:*.tex=93:*.zip=91:di=1;94'

antigen-use oh-my-zsh

antigen-bundle git
antigen-bundle djui/alias-tips
antigen-bundle vagrant
antigen-bundle zsh-users/zsh-syntax-highlighting
antigen-bundle zsh-users/zsh-history-substring-search
#antigen-bundle $ZSHA_BASE/bundles/stv
antigen bundle srijanshetty/zsh-pip-completion
antigen bundle mafredri/zsh-async

# theme
#antigen bundle sindresorhus/pure
antigen theme kolo

antigen-apply

# Alias
## xbps / Void Linux
alias xq='xbps-query -Rs'
alias xl='xbps-query -l'
alias xi='sudo xbps-install'
alias xu='sudo xbps-install -Su'
alias xr='sudo xbps-remove'
alias xo='xbps-query -O'
alias xS='xbps-query -RS'

## shutdown
alias tchau='sudo poweroff'

# SSH
alias S='ssh -l dandrade'
alias SR='ssh -l root'

alias v='vim'
alias n='ncmpcpp'

# virtualenvwrapper lazy
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Workspace
export VIRTUALENVWRAPPER_SCRIPT=/usr/bin/virtualenvwrapper.sh

