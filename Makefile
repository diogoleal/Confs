# Author: Diogo Leal - diogo@diogoleal.com

ST_VERSION = 0.7
DWM_VERSION = 6.1

pkgs:
	@xbps-install --yes zsh git vim-huge irssi tmux rxvt-unicode \
		rxvt-unicode-terminfo urxvt-perls git git-extras xbacklight feh xbanish \
			rtorrent htop
	@cp -rf .Xmodmap .Xresources .gitconfig .xinitrc .rtorrent.rc .irssi .config/htop ~/
	@cp -rf bin/ ~/bin/

dwm:
	@xbps-install libXinerama-devel libXft-devel freetype-devel
	@wget http://dl.suckless.org/dwm/dwm-${DWM_VERSION}.tar.gz
	@tar zxvf dwm-${DWM_VERSION}.tar.gz
	@cd dwm-${DWM_VERSION} && wget http://dwm.suckless.org/patches/dwm-6.1-systray.diff
	@cd dwm-${DWM_VERSION} && patch -p1 < dwm-6.1-systray.diff
	@cd dwm-${DWM_VERSION} && sed -i "/CFLAGS/s|\${CPPFLAGS}|& $CFLAGS|g" config.mk
	@cd dwm-${DWM_VERSION} && sed -i "/LDFLAGS/s|\-s|$LDFLAGS|g" config.mk
	@cd dwm-${DWM_VERSION} &&i make INCS="-I. -I/usr/include/freetype2" LIBS="-lX11 -lXinerama -lXft -lfontconfig"
	@cd dwm-${DWM_VERSION} && cp dwm ~/bin/ -f

st:
	@wget http://dl.suckless.org/st/st-${ST_VERSION}.tar.gz
	@tar zxf st-${ST_VERSION}.tar.gz
	cd st-${ST_VERSION} && wget http://st.suckless.org/patches/st-scrollback-0.7.diff
	cd st-${ST_VERSION} && patch -p1 < st-scrollback-0.7.diff
	cd st-${ST_VERSION} && sed -i s/pixelsize=12/pixelsize=14/g config.def.h
	cd st-${ST_VERSION} && wget http://st.suckless.org/patches/st-delkey-20160727-308bfbf.diff
	cd st-${ST_VERSION} && patch -p1 < st-delkey-20160727-308bfbf.diff
	cd st-${ST_VERSION} && make
	@cp st-${ST_VERSION}/st ~/bin
	cp st-${ST_VERSION}/st.info /usr/share/terminfo/s/st.terminfo
	rm -rf st-${ST_VERSION}.tar.gz st-${ST_VERSION}

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
	#@mkdir ~/.zsh-antigen
	@cp -v .zshrc ~/
	@curl -L https://raw.githubusercontent.com/zsh-users/antigen/master/antigen.zsh > ~/.antigen.zsh
	@source ~/.zshrc

.PHONY: all options clean dist install
