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
export VIRTUALENVWRAPPER_SCRIPT=/usr//bin/virtualenvwrapper.sh
source /usr/bin/virtualenvwrapper_lazy.sh

# vim-manpager
export MANPAGER="vim -c MANPAGER -"

# RVM
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
