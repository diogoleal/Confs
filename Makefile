DIR_CONF := $(HOME)/Workspace/Confs
BIN := $(HOME)/bin
LIB := $(HOME)/lib

.PHONY: all setup remove-upgrade install-deps fish flatpak kubectl go emacs kitty filen mega

rpm: setup fedora fish flatpak kubectl go emacs kitty
apt: setup fedora fish flatpak kubectl go emacs kitty

setup:
	mkdir -p $(BIN) $(LIB) $(HOME)/Workspace

ubuntu:
	sudo apt remove -y firefox rhythmbox
	sudo apt update && sudo apt upgrade -y
	sudo apt install -y direnv fzf fish ansible curl gnupg \
		bat duf procs ripgrep fd-find btop yt-dlp kitty \
    emacs libtool cmake clang-tools nodejs npm fonts-firacode
	sudo apt install -y flatpak gnome-software-plugin-flatpak
	sudo flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

fedora:
	sudo dnf remove -y firefox rhythmbox
	sudo dnf upgrade -y --refresh
	sudo dnf install -y direnv fzf fish ansible opentofu bat duf procs ripgrep \
		fd-find btop yt-dlp+default kitty
	sudo dnf install -y emacs libtool cmake clang-tools-extra nodejs npm fira-code-fonts
	sudo dnf install -y https://cdn.filen.io/@filen/desktop/release/latest/Filen_linux_x86_64.rpm

fish:
	mkdir -p ~/.config/fish/{functions,completions}
	ln -sf $(DIR_CONF)/.gitconfig $(HOME)/.gitconfig
	ln -sf $(DIR_CONF)/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
	ln -sf $(DIR_CONF)/.config/fish/config.fish ~/.config/fish/config.fish
	ln -sf $(DIR_CONF)/.config/fish/k9s.fish ~/.config/fish/completions/k9s.fish
	sudo chsh -s /usr/bin/fish $$(whoami)

flatpak:
	flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
	flatpak install flathub -y  org.telegram.desktop \
	                            com.vivaldi.Vivaldi \
	                            io.ente.auth \
	                            dev.geopjr.Tuba \
	                            org.onlyoffice.desktopeditors \
	                            org.gnome.World.PikaBackup \
	                            net.jami.Jami

kubectl:
	curl -LO https://dl.k8s.io/release/v1.32.0/bin/linux/amd64/kubectl
	install -m=+x+r kubectl $(BIN)/kubectl
	rm -f kubectl

go:
	curl -s https://go.dev/VERSION?m=text | cut -d' ' -f1 | grep -v time > .goversion
	curl -LO https://go.dev/dl/$$(cat .goversion).linux-amd64.tar.gz
	tar -C $(BIN)/ -xzf $$(cat .goversion).linux-amd64.tar.gz
	rm -f $$(cat .goversion).linux-amd64.tar.gz .goversion

emacs:
	ln -sf $(DIR_CONF)/.emacs.d/init.el $(HOME)/.emacs.d/init.el
	go install golang.org/x/tools/gopls@latest
	npm install bash-language-server yaml-language-server pyright

kitty:
	ln -sf $(DIR_CONF)/.config/kitty/kitty.conf ~/.config/kitty/kitty.conf

