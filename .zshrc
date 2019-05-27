
CASE_SENSITIVE="false"
source ~/.zshenv
source ~/.zsh/aliases

source ~/bin/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
#antigen bundle heroku
antigen bundle pip
#antigen bundle lein
antigen bundle command-not-found
antigen bundle pip
antigen bundle colorize

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme robbyrussell
#antigen theme pure
# Tell Antigen that you're done.
antigen apply

#direnv
eval "$(direnv hook zsh)"