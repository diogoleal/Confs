ZSHA_BASE=$HOME/.zsh-antigen
source $ZSHA_BASE/antigen/antigen.zsh

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
antigen bundle sindresorhus/pure

antigen-apply
