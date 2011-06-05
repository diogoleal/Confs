XDG_CONFIG_DIRS="/etc/xdg:$HOME/.local/etc/xdg"
XDG_DATA_DIRS="/usr/share:/usr/local/share:$HOME/.local/share"
export XDG_CONFIG_DIRS XDG_DATA_DIRS

#Define variavel de screen para vim
export SCREENSIZE=$(xdpyinfo 2>/dev/null 2>/dev/null | grep 'dimensions' | sed -e 's/x.*//g' -e 's/^.*[a-z]: *//g')

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

#Alias
alias ls='ls --color'
alias site='ssh diogo@diogoleal.com -p 22'
alias it='cd ~/Stuff/Intelie/itsm'
alias gp='git pull'
alias gh='git push'
alias rackspace='ssh root@173.203.59.21'
alias slides='cd ~/Stuff/Slides/'
alias l='less'
alias gc='git clone'
alias gk='gitk &'
alias g='gvim '

#bindkey
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char

PROMPT="%n@%m % %~ %# "
