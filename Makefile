# Author: Diogo Leal - diogo@diogoleal.com

ST_VERSION = 0.7
ST_PATH = st-${ST_VERSION}
BIN = "~/bin/"
ZSH_DIR = "~/.zsh"
XI = sudo xbps-install --yes

pkgs:
	${XI} xterm xorg xbacklight feh xbanish rtorrent firefox mpv sutils git xorg slock htop perf
	@cp -rf bin/ ~/bin/

qutebrowser:
	${XI} qutebrowser qt5-webengine
	@cp -rv .config/qutebrowser ~/.config

cron:
	${XI} scron
	@cp -rv etc/crontab /etc/

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
	@cp -rf .Xmodmap .Xresources .xinitrc .rtorrent.rc .config/htop/ ~/

pulseaudio:
	${XI} pulseaudio pavucontrol alsa-utils ConsoleKit2
	sudo ln -s /etc/sv/alsa /var/service/
	sudo ln -s /etc/sv/dbus /var/service/
	sudo ln -s /etc/sv/cgmanager /var/service/
	sudo ln -s /etc/sv/consolekit /var/service/
	sudo usermod -a -G pulse-access ${USER}

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
	@cp -r .config/polybar ~/.config/

bspwm:
	${XI} bspwm sxhkd
	cp -rv .config/bspwm ~/.config/
	cp -rv .config/sxhkd ~/.config/

rofi:
	${XI} rofi
	cp -r .config/rofi ~/.config/

noice:
	${XI} ncurses-devel
	@git clone git://git.2f30.org/noice.git
	@cd noice && make && make install
	@sudo cp noice/noice.1 /usr/share/man/
	@cp noice ~/bin/

st:
	#@sudo xbps-install fontconfig-devel libX11-devel libXft-devel
	@wget http://dl.suckless.org/st/st-${ST_VERSION}.tar.gz
	@tar zxf st-${ST_VERSION}.tar.gz
	cd st-${ST_VERSION} && wget http://st.suckless.org/patches/st-scrollback-0.7.diff
	cd st-${ST_VERSION} && patch -p1 < st-scrollback-0.7.diff
	cd st-${ST_VERSION} && sed -i s/pixelsize=12/pixelsize=14/g config.def.h
	cd st-${ST_VERSION} && wget http://st.suckless.org/patches/st-delkey-20160727-308bfbf.diff
	cd st-${ST_VERSION} && patch -p1 < st-delkey-20160727-308bfbf.diff
	cd st-${ST_VERSION} && make
	@cp st-${ST_VERSION}/st ~/bin
	sudo cp -fv st-${ST_VERSION}/st.info /usr/share/terminfo/s/st.terminfo
	rm -rf st-${ST_VERSION}.tar.gz st-${ST_VERSION}

tmux:
	${XI} tmux
	@git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
	@cp -v .tmux.conf ~/.tmux.conf

vim:
	${XI} neovim
	@mkdir ~/.vim
	@cp -v .vimrc ~/
	@touch ~/.simplenoterc
	@ln -s ~/.vimrc ~/.config/nvim/init.vim
	@curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@vim +PlugInstall +qall

xbps:
	@cp -rfv usr/share/xbps.d/xbps.conf /usr/share/xbps.d/xbps.conf

fish:
	${XI} fish-shell
	@curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
	fisher transfer eco
	fisher install laughedelic/pisces

zsh:
	${XI} zsh
	mkdir -p ${ZSH_DIR}
	@git clone https://github.com/sindresorhus/pure ${ZSH_DIR}
	@git clone git://github.com/zsh-users/zsh-autosuggestions ${ZSH_DIR}
	@git clone git://github.com/zsh-users/zsh-completions.git ${ZSH_DIR}
	cp -v .zshrc .zshenv .zlogin ~/
	source ~/.zshrc
	chsh -s /bin/zsh ${USER}

#.PHONY? all
	#sudo xbps-install --yes git
	#all: $(pkgs) $(zsh) $(fish) $(xbps)

clean:
	@rm -rf ${ST_PATH}.tar.gz ${ST_PATH}
