
BIN = "~/bin/"
ZSH_DIR = "~/.zsh"

confs:
	@cp -rf .config/htop ~/
	@cp -rf .gitconfig ~/

confs:
	@cp -rf .Xmodmap .Xresources .xinitrc .rtorrent.rc .config/htop/ .Xresources.d ~/

tmux:
	@git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
	@cp -v .tmux.conf ~/.tmux.conf

vim:
	@mkdir -p ~/.vim && true
	@mkdir -p ~/.config/nvim && true
	@cp -v .vimrc ~/
	@ln -s ~/.vimrc ~/.config/nvim/init.vim
	@curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@vim +PlugInstall +qall

fish:
	@curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
	fisher transfer eco
	fisher install laughedelic/pisces

zsh:
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
