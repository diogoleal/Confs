#!/bin/bash

## Vim keybindings
# * <kbd>F2</kbd> - nerdtree
# * <kbd>F3</kbd> - set for using tab for indentation
# * <kbd>F9</kbd> - set for using space for indentation
# * <kbd>F5</kbd> - toggle paste mode on and off
# * <kbd>F6</kbd> - Nuake Terminal
# * <kbd>F8</kbd> - clean highlight search
# * <kbd>F9</kbd> - Show/hide comments
# * <kbd>F11</kbd> - tagbar
# * <kbd>C-e</kbd> =>  + <kbd>h</kbd>, <kbd>j</kbd>, <kbd>k</kbd>, <kbd>l</kbd>  - winresize
# * <kbd>C-n</kbd> - autocomplete

mkdir -p "$HOME"/.config/nvim && true
ln -s "$DIR_CONF"/.vimrc "$HOME"/.config/nvim/init.vim
curl -sfLo "$HOME"/.local/share/nvim/site/autoload/plug.vim --create-dirs \
https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim +PlugInstall +qall
