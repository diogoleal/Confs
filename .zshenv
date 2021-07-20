# Define Zim location
: ${ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim}
# }}} End configuration added by Zim install

typeset -gx -U path
path=( \
    /usr/local/bin(N-/) \
    ~/bin(N-/) \
    ~/.zplug/bin(N-/) \
    ~/.tmux/bin(N-/) \
    "$path[@]" \
    )

# NOTE: set fpath before compinit
typeset -gx -U fpath
fpath=( \
    ~/.zsh/Completion(N-/) \
    ~/.zsh/functions(N-/) \
    ~/.zsh/zsh-completions(N-/) \
    /usr/local/share/zsh/site-functions(N-/) \
    $fpath \
    )

# autoload
autoload -Uz run-help
autoload -Uz add-zsh-hook
autoload -Uz colors && colors
autoload -Uz compinit && compinit -u
autoload -Uz is-at-least

# LANGUAGE must be set by en_US
export LANGUAGE="en_US.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

# GoLang Path
export GOPATH=$HOME/Workspace/go
export PATH=$PATH:$GOPATH/bin

# Export ~/bin
export PATH="$PATH:$HOME/bin"

# Pyenv
export PATH="$PATH:$HOME/.pyenv/bin:$PATH"
eval "$(pyenv virtualenv-init -)"

export WORKON_HOME="${HOME}/.ve"
export PROJECT_HOME="${HOME}/Workspace"
eval "$(pyenv init -)"

#pipenv
export PATH="$PATH:$HOME/.local/bin:$PATH"

# Editor
export EDITOR=nvim
export CVSEDITOR="${EDITOR}"
export SVN_EDITOR="${EDITOR}"
export GIT_EDITOR="${EDITOR}"

# Pager
export PAGER="less -R -N"
# Less status line
export LESS='-R -f -X -i -P ?f%f:(stdin). ?lb%lb?L/%L.. [?eEOF:?pb%pb\%..]'
export LESSCHARSET='utf-8'

# LESS man page colors (makes Man pages more readable).
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[00;44;37m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# ls command colors
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

setopt no_global_rcs
# Add ~/bin to PATH
export PATH=~/bin:"$PATH"

# Settings for golang
export GOPATH="$HOME"
export GOBIN="$GOPATH/bin"
export PATH="$GOBIN:$PATH"

# declare the environment variables
export CORRECT_IGNORE='_*'
export CORRECT_IGNORE_FILE='.*'

export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export WORDCHARS='*?.[]~&;!#$%^(){}<>'

# History file and its size
export HISTFILE=~/.zsh_history
export HISTSIZE=1000000
export SAVEHIST=1000000

# fzf - command-line fuzzy finder (https://github.com/junegunn/fzf)
export FZF_DEFAULT_OPTS="--extended --ansi --multi"

# available $INTERACTIVE_FILTER
export INTERACTIVE_FILTER="fzf:peco:percol:gof:pick"



