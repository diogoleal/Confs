ZSHA_BASE=$HOME/.zsh-antigen
source $ZSHA_BASE/antigen.zsh

# Alias
alias dv='DVTM_TERM=urxvtc dvtm'

antigen-use oh-my-zsh
antigen-bundle git

antigen-bundle djui/alias-tips

antigen-bundle vagrant
antigen-bundle zsh-users/zsh-syntax-highlighting
antigen-bundle zsh-users/zsh-history-substring-search
#antigen-bundle $ZSHA_BASE/bundles/stv
#antigen theme zenorocha/dracula-theme
antigen bundle srijanshetty/zsh-pip-completion
antigen bundle mafredri/zsh-async
#antigen bundle sindresorhus/pure
antigen theme kolo
antigen-apply

# virtualenvwrapper lazy
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Workspace
export VIRTUALENVWRAPPER_SCRIPT=/usr//bin/virtualenvwrapper.sh
source /usr/bin/virtualenvwrapper_lazy.sh

