#!/bin/sh

# Script for nstall my confs

# Install Oh-my-zsh
git clone $ curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh

# tmux
git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack

cp .Xmodmap .Xresources .emacs  .gitconfig .muttrc .tmux.conf .vimrc .xinitrc .zshrc ~/

mkdir ~/.vim
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

#git clone https://github.com/morhetz/gruvbox.git
#mv gruvbox/colors gruvbox/autoload ~/.vim/
#rm -rf gruvbox

#git clone https://github.com/Valloric/YouCompleteMe.git
vim +PluginInstall +qall

git clone http://git.sysphere.org/vicious ~/.config/awesome/
cp -rf .config/awesome/* ~/.config/awesome

