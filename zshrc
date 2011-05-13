# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle :compinstall filename '/home/diogo/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

#The function will not be run in future, but you can run
#it yourself as follows:
#  autoload zsh-newuser-install
#    zsh-newuser-install -f

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000
# End of lines configured by zsh-newuser-install
alias ls='ls --color'
alias site='ssh diogo@diogoleal.com -p 2222'

XDG_CONFIG_DIRS="/etc/xdg:$HOME/.local/etc/xdg"
XDG_DATA_DIRS="/usr/share:/usr/local/share:$HOME/.local/share"

export XDG_CONFIG_DIRS XDG_DATA_DIRS

PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m%{$fg[yellow]%}%1~%{$reset_color%}%# "
#RPROMPT="[%{$fg[yellow]%}%?%{$reset_color%}]"

