#!/bin/sh

export BIN="$HOME/bin/"
ZSH_DIR="$HOME/.zsh"

__f_config(){
  cp -rf .config/htop .gitconfig ~/
  #cp -rf .Xmodmap .Xresources .xinitrc .rtorrent.rc .config/htop/ .Xresources.d ~/
  git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
  cp -v .tmux.conf ~/.tmux.conf
}

__f_vim(){
  mkdir -p ~/.vim && true
  mkdir -p ~/.config/nvim && true
  cp -v .vimrc ~/
  ln -s ~/.vimrc ~/.config/nvim/init.vim
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  vim +PlugInstall +qall
}

__f_fish(){
  curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  fisher install laughedelic/pisces
  python3 -m pip install --user pipx
  register-python-argcomplete --shell fish pipx >~/.config/fish/completions/pipx.fish
  pipx install virtualfish
  fish_add_path ~/.local/bin
  # exec fish
  vf install compat_aliases projects environment
  exec fish
}

__f_zsh(){
  mkdir -p "${ZSH_DIR}"
  git clone https://github.com/sindresorhus/pure "${ZSH_DIR}"
  git clone git://github.com/zsh-users/zsh-autosuggestions "${ZSH_DIR}"
  git clone git://github.com/zsh-users/zsh-completions.git "${ZSH_DIR}"
  cp -v .zshrc .zshenv .zlogin ~/
  . "${HOME}"/.zshrc
  chsh -s /bin/zsh "${USER}"
}

option="${1}"

case "${option}" in
  "vim")
    __f_vim
    ;;
  "fish")
    __f_fish
    ;;
  "zsh")
    __f_zsh
    ;;
  "all")
    __f_vim
    __f_fish
     ;;
   *)
     echo " build.sh vim | fish | zsh | all"
     exit 1
    ;;
esac