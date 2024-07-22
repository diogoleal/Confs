#!/bin/bash

# # neovim
mkdir -p "$HOME"/.config/nvim && true
ln -s "$DIR_CONF"/.vimrc "$HOME"/.config/nvim/init.vim
curl -sfLo "$HOME"/.local/share/nvim/site/autoload/plug.vim --create-dirs \
https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim +PlugInstall +qall
