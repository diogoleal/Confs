#!/bin/sh
# Script for install my confs


# xbps conf
cp -rfv usr/share/xbps.d/xbps.conf /usr/share/xbps.d/xbps.conf

pkgs='
    mutt-kz
    zsh
    git
    mpd
    vim-huge
    irssi
    ncmpcpp
    i3
    i3lock
    i3status
    tmux
    rxvt-unicode
    rxvt-unicode-terminfo
    urxvt-perls
    git-extras
    tig
    wicd
    xombrero
    xbacklight
    feh
    xbanish
    rtorrent
    newsbeuter
    task
    htop
    profanity
'
echo $pkgs
xbps-install --yes $pkgs

# Zsh
mkdir ~/.zsh-antigen
cp -v .zshrc ~/
curl -L https://raw.githubusercontent.com/zsh-users/antigen/master/antigen.zsh > ~/.zsh-antigen/antigen.zsh

# tmux
git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
cp -v .tmux.conf ~/.tmux.conf

# Vim
mkdir ~/.vim
cp -v .vimrc ~/
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# Others Confs
cp -vrf .Xmodmap .Xresources .gitconfig .xinitrc .xombrero .rtorrent.rc .irssi .taskrc\
.muttrc .mpd.conf .config/htop .newsbeuter ~/

#my bin files
cp bin/ ~/bin/

#i3
cp -rfv .i3 .i3status.conf ~/

