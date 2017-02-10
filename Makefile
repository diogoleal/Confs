# Author: Diogo Leal - diogo@diogoleal.com

pkgs:
	@xbps-install --yes zsh git vim-huge irssi ncmpcpp tmux rxvt-unicode \
		rxvt-unicode-terminfo urxvt-perls git git-extras xbacklight feh xbanish \
			rtorrent task htop
	@cp -rf .Xmodmap .Xresources .gitconfig .xinitrc .rtorrent.rc .irssi .taskrc\
	.mpd.conf .config/htop  ~/
	@cp -rf bin/ ~/bin/

dwm: ${OBJ}
	@xbps-install libXinerama-devel libXft-devel freetype-devel
	@wget http://dl.suckless.org/dwm/dwm-6.1.tar.gz
	@tar zxvf dwm-6.1.tar.gz
	@cd dwm-6.1
	@wget http://dwm.suckless.org/patches/dwm-6.1-systray.diff
	@patch -p1 < dwm-6.1-systray.diff
	@sed -i "/CFLAGS/s|\${CPPFLAGS}|& $CFLAGS|g" config.mk
	@sed -i "/LDFLAGS/s|\-s|$LDFLAGS|g" config.mk
	@make INCS="-I. -I/usr/include/freetype2" LIBS="-lX11 -lXinerama -lXft -lfontconfig"
	@cp dwm ~/bin/ -f

tmux:
	@git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
	@cp -v .tmux.conf ~/.tmux.conf

vim:
	@mkdir ~/.vim
	@cp -v .vimrc ~/
	@git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	@vim +PluginInstall +qall

xbps:
	@cp -rfv usr/share/xbps.d/xbps.conf /usr/share/xbps.d/xbps.conf

zsh:
	@mkdir ~/.zsh-antigen
	@cp -v .zshrc ~/
	@curl -L https://raw.githubusercontent.com/zsh-users/antigen/master/antigen.zsh > ~/.zsh-antigen/antigen.zsh

.PHONY: all options clean dist install
