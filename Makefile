# Author: Diogo Leal - diogo@diogoleal.com

ST_VERSION = 0.7
ST_PATH = st-${ST_VERSION}
DWM_VERSION = 6.1
DWM_PATH = dwm-${DWM_VERSION}
BIN = "~/bin/"
ZSH_DIR = "~/.zsh"
XI = sudo xbps-install --yes

pkgs:
	${XI} xbacklight feh xbanish rtorrent firefox mpv sutils slock
	@cp -rf bin/ ~/bin/

qutebrowser:
	${XI} qutebrowser qt5-webengine
	@cp -rv .config/qutebrowser ~/.config

irssi:
	${XI} irssi irssi-perl
	@cp -rf .irssi ~/

htop:
	${XI} htop
	@cp -rf .config/htop ~/

git:
	${XI} git git-extras
	cp -rf .gitconfig ~/

confs:
	@cp -rf .Xmodmap .Xresources .xinitrc .rtorrent.rc  ~/

pulseaudio:
	${XI} pulseaudio pavucontrol
	ln -s /etc/sv/pulseaudio/ /var/service/

log:
	${XI} socklog-void
	usermod -aG socklog diogo
	ln -s /etc/sv/socklog-unix /var/service/
	ln -s /etc/sv/nanoklogd /var/service/

polybar:
	@git clone --branch 3.0.5 --recursive https://github.com/jaagr/polybar
	@mkdir polybar/build
	@cd polybar/build && cmake ..
	@sudo make install
	@cp -r .config/polybar ~/.config

bspwm:
	${XI} bspwm sxhkd
	cp -rv .config/bspwm ~/.config
	cp -rv .config/sxhkd ~/.config

rofi:
	${XI} rofi
	cp -r .config/rofi ~/.config

dwm:
#	@sudo xbps-install libXinerama-devel libXft-devel freetype-devel
	@wget http://dl.suckless.org/dwm/dwm-${DWM_VERSION}.tar.gz
	@tar zxvf dwm-${DWM_VERSION}.tar.gz
	@cd dwm-${DWM_VERSION} && wget http://dwm.suckless.org/patches/dwm-6.1-systray.diff
	@cd dwm-${DWM_VERSION} && wget http://dwm.suckless.org/patches/dwm-autoresize-6.1.diff
	@wget http://dwm.suckless.org/patches/dwm-gridmode-5.8.2.diff -O ${DWM_PATH}/dwm-gridmode-5.8.2.diff
	@wget http://dwm.suckless.org/patches/dwm-pertag-6.1.diff -O ${DWM_PATH}/dwm-pertag-6.1.diff
	@cd dwm-${DWM_VERSION} && patch -p1 < dwm-6.1-systray.diff
	@cd dwm-${DWM_VERSION} && patch -p1 < dwm-autoresize-6.1.diff
	@cd dwm-${DWM_VERSION} && patch -p1 < dwm-gridmode-5.8.2.diff
	@cd dwm-${DWM_VERSION} && patch -p1 < dwm-pertag-6.1.diff
	@cp config.def.h.my.patch ${DWM_PATH}
	@cd ${DWM_PATH} &&  patch -p1 config.def.h < config.def.h.my.patch
#	@sed -i "/CFLAGS/s|\${CPPFLAGS}|& $CFLAGS|g" dwm-6.1/config.mk
#	@sed -i "/CFLAGS/s|\${CPPFLAGS}|& $CFLAGS|g" ${DWM_VERSION}/config.mk
#	@cd dwm-${DWM_VERSION} && sed -i "/LDFLAGS/s|\-s|$LDFLAGS|g" config.mk
#	@cd dwm-${DWM_VERSION} && make INCS="-I. -I/usr/include/freetype2" LIBS="-lX11 -lXinerama -lXft -lfontconfig"
#	@cd dwm-${DWM_VERSION} && cp dwm ~/bin/ -f

st:
	@sudo xbps-install fontconfig-devel libX11-devel libXft-devel
	@wget http://dl.suckless.org/st/st-${ST_VERSION}.tar.gz
	@tar zxf st-${ST_VERSION}.tar.gz
	cd st-${ST_VERSION} && wget http://st.suckless.org/patches/st-scrollback-0.7.diff
	cd st-${ST_VERSION} && patch -p1 < st-scrollback-0.7.diff
	cd st-${ST_VERSION} && sed -i s/pixelsize=12/pixelsize=14/g config.def.h
	cd st-${ST_VERSION} && wget http://st.suckless.org/patches/st-delkey-20160727-308bfbf.diff
	cd st-${ST_VERSION} && patch -p1 < st-delkey-20160727-308bfbf.diff
	cd st-${ST_VERSION} && make
	@cp st-${ST_VERSION}/st ~/bin
	cp -fv st-${ST_VERSION}/st.info /usr/share/terminfo/s/st.terminfo
	rm -rf st-${ST_VERSION}.tar.gz st-${ST_VERSION}

noice:
	sudo xbps-install ncurses-devel
	git clone git://git.2f30.org/noice.git
#	cd noice && patch -p1 config.def.h << ../patches/
#	cp noice ~/bin/

tmux:
	${XI} tmux
	@git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
	@cp -v .tmux.conf ~/.tmux.conf

vim:
	${XI} neovim
	@mkdir ~/.vim
	@cp -v .vimrc ~/
	@touch ~/.simplenoterc
	@curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@vim +PlugInstall +qall

xbps:
	@cp -rfv usr/share/xbps.d/xbps.conf /usr/share/xbps.d/xbps.conf

zsh:
	${XI} zsh
	mkdir ${ZSH_DIR}
	@git clone https://github.com/sindresorhus/pure ${ZSH_DIR}
	@git clone git://github.com/zsh-users/zsh-autosuggestions ${ZSH_DIR}
	@git clone git://github.com/zsh-users/zsh-completions.git ${ZSH_DIR}
	cp -v .zshrc .zshenv .zlogin ~/
	source ~/.zshrc
	chsh -s /bin/zsh ${USER}

clean:
	@rm -rf ${DWM_PATH} ${DWM_PATH}.tar.gz ${ST_PATH}.tar.gz ${ST_PATH}
