
typeset -gx -U path
path=( \
    /usr/local/bin(N-/) \
     ~/.tmux/bin(N-/) \
    "$path[@]" \
    )

# LANGUAGE must be set by en_US
export LANGUAGE="en_US.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

# Go
export GOPATH=$HOME/go
export GOBIN="$GOPATH/bin"
export PATH="$GOBIN:$PATH"

# Export ~/bin
export PATH="$PATH:$HOME/bin"
export PATH=$PATH:$HOME/
export PATH=$PATH:$HOME/.cargo/bin

# export PATH="$PATH:$HOME/.local/bin:$PATH"
# Pyenv

export PYENV_ROOT="$HOME/.pyenv"
export PYTHON_BUILD_CACHE_PATH="$PYENV_ROOT/cache"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
# export WORKON_HOME="${HOME}/.ve"
# export PROJECT_HOME="${HOME}/Workspace"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Editor
export EDITOR="emacsclient -t"
export CVSEDITOR="${EDITOR}"
export SVN_EDITOR="${EDITOR}"
export GIT_EDITOR="${EDITOR}"

# Pager
# export PAGER="less -R -N"

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
# export lscolors=exfxcxdxbxegedabagacad
# export ls_colors='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

setopt no_global_rcs

export correct_ignore='_*'
export correct_ignore_file='.*'
export wordchars='*?_-.[]~=&;!#$%^(){}<>'
export wordchars='*?.[]~&;!#$%^(){}<>'

# history file and its size
export histfile=~/.zsh_history
export histsize=1000000
export savehist=1000000

# fzf - command-line fuzzy finder (https://github.com/junegunn/fzf)
export fzf_default_opts="--extended --ansi --multi"

# available $interactive_filter
export interactive_filter="fzf:peco:percol:gof:pick"

export sdkman_dir="$home/.sdkman"
[[ -s "$home/.sdkman/bin/sdkman-init.sh" ]] && source "$home/.sdkman/bin/sdkman-init.sh"

# k8s
source <(kubectl completion zsh)
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

autoload -Uz compinit
compinit

