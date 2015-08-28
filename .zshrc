ZSHA_BASE=$HOME/.zsh-antigen
source $ZSHA_BASE/antigen.zsh

antigen-use oh-my-zsh

antigen-bundle git
antigen-bundle djui/alias-tips
antigen-bundle vagrant
antigen-bundle zsh-users/zsh-syntax-highlighting
antigen-bundle zsh-users/zsh-history-substring-search
#antigen-bundle $ZSHA_BASE/bundles/stv
antigen bundle srijanshetty/zsh-pip-completion
antigen bundle mafredri/zsh-async

#theme
#antigen bundle sindresorhus/pure
antigen theme kolo

antigen-apply

#Alias
## xbps / Void Linux
alias xi='sudo xbps-install'
alias xq='xbps-query -Rs'
alias xu='sudo xbps-install -Su'
alias xr='sudo xbps-remove'
## shutdown 
alias tchau='sudo poweroff'

alias v='vim'

# virtualenvwrapper lazy
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Workspace
export VIRTUALENVWRAPPER_SCRIPT=/usr//bin/virtualenvwrapper.sh
source /usr/bin/virtualenvwrapper_lazy.sh

# vim-manpager
export MANPAGER="vim -c MANPAGER -"
