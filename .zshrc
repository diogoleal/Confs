
autoload -Uz compinit
compinit

CASE_SENSITIVE="false"

HISTSIZE=10000
HISTFILE=~/.zsh_history
SAVEHIST=10000

source ~/.zsh/aliases
source ~/.profile
source ~/.zsh/zsh-completions/zsh-completions.plugin.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/pure/async.zsh
source ~/.zsh/pure/pure.zsh

bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '\e[3~' delete-char

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  startx
fi
