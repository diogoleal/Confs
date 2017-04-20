
autoload -Uz compinit
compinit

zstyle ':completion:*' rehash true
zstyle ':completion:*' menu select
setopt COMPLETE_ALIASES

CASE_SENSITIVE="false"

HISTSIZE=10000
HISTFILE=~/.zsh_history
SAVEHIST=10000

ttyctl -f

source ~/.zsh/aliases
source ~/.profile
source ~/.zsh/zsh-completions/zsh-completions.plugin.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/pure/async.zsh
source ~/.zsh/pure/pure.zsh

bindkey -v
bindkey '^R' history-incremental-search-backward
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '\e[3~' delete-char


if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  startx
fi
