
# Initialise zulu plugin manager
source "${ZULU_DIR:-"${ZDOTDIR:-$HOME}/.zulu"}/core/zulu"
zulu init


# # autoload
# autoload -Uz run-help
# autoload -Uz add-zsh-hook
# autoload -Uz colors && colors
# autoload -Uz compinit && compinit -u
# autoload -Uz is-at-least


# # NOTE: set fpath before compinit
# typeset -gx -U fpath
# fpath=( \
#     ~/.zsh/Completion(N-/) \
#     ~/.zsh/functions(N-/) \
#     ~/.zsh/zsh-completions(N-/) \
#     /usr/local/share/zsh/site-functions(N-/) \
#     $fpath \
#     )


# Remove older command from the history if a duplicate is to be added.
# setopt HIST_IGNORE_ALL_DUPS

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
# bindkey -e

# Prompt for spelling correction of commands.
# setopt CORRECT

# Remove path separator from WORDCHARS.
# WORDCHARS=${WORDCHARS//[\/]}
# ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
# ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=032"
# ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# # Bind ^[[A/^[[B manually so up/down works both before and after zle-line-init
# bindkey '^[[A' history-substring-search-up
# bindkey '^[[B' history-substring-search-down

# # Bind up and down keys
# zmodload -F zsh/terminfo +p:terminfo
# if [[ -n ${terminfo[kcuu1]} && -n ${terminfo[kcud1]} ]]; then
#   bindkey ${terminfo[kcuu1]} history-substring-search-up
#   bindkey ${terminfo[kcud1]} history-substring-search-down
# fi

# bindkey '^P' history-substring-search-up
# bindkey '^N' history-substring-search-down
# bindkey -M vicmd 'k' history-substring-search-up
# bindkey -M vicmd 'j' history-substring-search-down

# asdf
source ${HOME}/.asdf/asdf.sh
# append completions to fpath
fpath=(${ASDF_DIR}/completions $fpath)
# initialise completions with ZSH's compinit
autoload -Uz compinit && compinit


# source ${HOME}/.secrets

# # source ${HOME}/.zsh/anaconda.zsh

eval "$(direnv hook zsh)"

# export PATH="$PATH:${HOME}/.linuxbrew/bin:$PATH"

# export TALISMAN_HOME=${HOME}/.talisman/bin

# #THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
# export SDKMAN_DIR="${HOME}/.sdkman"
# [[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ]] && source "${HOME}/.sdkman/bin/sdkman-init.sh"

# eval "$(oh-my-posh --init --shell zsh --config ${HOME}/.zsh/material.omp.json)"
# enable_poshtooltips


