export ZSH=$HOME/.zsh

### ---- history config -------------------------------------
export HISTFILE=$ZSH/.zsh_history

# How many commands zsh will load to memory.
export HISTSIZE=10000

# How many commands history will save on file.
export SAVEHIST=10000

# History won't save duplicates.
setopt HIST_IGNORE_ALL_DUPS

# History won't show duplicates on search.
setopt HIST_FIND_NO_DUPS

setopt INC_APPEND_HISTORY

# Alias
source ${HOME}/Workspace/Confs/.zsh_alias
source ${HOME}/Workspace/Confs/.zshenv

# asdf
source ${HOME}/.asdf/asdf.sh
# append completions to fpath
#fpath=(${ASDF_DIR}/completions $fpath)
# initialise completions with ZSH's compinit
#autoload -Uz compinit && compinit

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
  source /etc/profile.d/vte.sh
fi

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /home/diogo/.asdf/installs/terraform/1.2.8/bin/terraform terraform

### ---- PLUGINS & THEMES -----------------------------------
source $ZSH/themes/spaceship-prompt/spaceship.zsh-theme

### --- Spaceship Config ------------------------------------

SPACESHIP_PROMPT_ORDER=(
  user          # Username section
  dir           # Current directory section
  host          # Hostname section
  git           # Git section (git_branch + git_status)
  hg            # Mercurial section (hg_branch  + hg_status)
  exec_time     # Execution time
  line_sep      # Line break
  vi_mode       # Vi-mode indicator
  jobs          # Background jobs indicator
  exit_code     # Exit code section
  char          # Prompt character
)
SPACESHIP_USER_SHOW=always
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_CHAR_SYMBOL="❯"
SPACESHIP_CHAR_SUFFIX=" "

### ---- PLUGINS & THEMES -----------------------------------
source $ZSH/themes/spaceship-prompt/spaceship.zsh-theme
source $ZSH/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source $ZSH/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fpath=($ZSH/plugins/zsh-completions/src $fpath)

