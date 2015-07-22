#!/bin/sh
# Script for install my confs

# Zsh
mkdir ~/.zsh-antigen
cp -v .zshrc ~/
curl -L https://raw.githubusercontent.com/zsh-users/antigen/master/antigen.zsh > ~/.zsh-antigen/antigen.zsh

# tmux
git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
cp -v .tmux.conf ~/.tmux.conf

# Vim
cp -v .vimrc ~/
mkdir ~/.vim
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# Awesome
git clone http://git.sysphere.org/vicious ~/.config/awesome/
cp -rfv .config/awesome/* ~/.config/awesome

# Irssi
cp -rfv .irssi ~/

# Others Confs
cp -v .Xmodmap .Xresources .gitconfig .xinitrc ~/

