
source ~/.zshenv
source ~/.zsh/aliases
source ~/bin/antigen.zsh

antigen bundles <<EOBUNDLES
  command-not-found
  zsh-users/zsh-syntax-highlighting
  sublime
  pip
  colorize
  git
  python
EOBUNDLES

# Load the theme.
antigen theme denysdovhan/spaceship-prompt
antigen apply
