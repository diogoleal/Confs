DIR_CONF := $(HOME)/Workspace/Confs
BIN := $(HOME)/bin
LIB := $(HOME)/lib

all: setup arch fish kubectl emacs kitty virt
pacman: all

.PHONY: all setup fish flatpak kubectl go emacs kitty virt krew-install pacman ubuntu fedora arch clean

krew-install:
	@set -x; \
	TMP_DIR=$$(mktemp -d); \
	cd $$TMP_DIR && \
	OS=$$(uname | tr '[:upper:]' '[:lower:]') && \
	ARCH=$$(uname -m | sed -e 's/x86_64/amd64/' \
	                        -e 's/\(arm\)\(64\)\?.*/\1\2/' \
	                        -e 's/aarch64$$/arm64/') && \
	KREW="krew-$${OS}_$${ARCH}" && \
	curl -fsSLO "https://github.com/kubernetes-sigs/krew/releases/latest/download/$${KREW}.tar.gz" && \
	tar zxvf "$${KREW}.tar.gz" && \
	./"$${KREW}" install krew

rpm: setup fedora fish flatpak kubectl go emacs kitty
apt: setup ubuntu fish flatpak kubectl go emacs kitty

arch:
	sudo pacman -Syu --needed --noconfirm ttf-cascadia-code-nerd fish \
		emacs kitty direnv fzf btop bat duf ripgrep pyenv \
		yt-dlp niri vivaldi vorta mupdf autossh \
		virt-manager nerdctl rootlesskit packagekit \
		jami-qt discord telegram-desktop fluxcd aws-cli-v2 \
		cups cups-pk-helper kimageformats ddcutil \
		i2c-tools qt6ct qt5ct kustomize \
		clang python-lsp-server bash-language-server nodejs shfmt \
		yaml-language-server pyright vscode-json-languageserver gopls

	yay -S --noconfirm --answerclean \
		xfe cockatrice dsearch-bin

	sudo systemctl enable --now bluetooth

	sudo mkdir -p /etc/pacman.d/hooks && \
		sudo ln -sf $(DIR_CONF)/etc/pacman.d/hooks/clean-cache.hook /etc/pacman.d/hooks/clean-cache.hook
	sudo sed -i 's/^#\?CleanMethod = .*/CleanMethod = KeepCurrent/' /etc/pacman.conf

setup:
	mkdir -p $(BIN) $(LIB) $(HOME)/Workspace
	ln -sf $(DIR_CONF)/.gitconfig $(HOME)/.gitconfig

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
	curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
	ln -sf $(DIR_CONF)/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
	ln -sf $(DIR_CONF)/.config/fish/config.fish ~/.config/fish/config.fish
	ln -sf $(DIR_CONF)/.config/fish/k9s.fish ~/.config/fish/completions/k9s.fish
	sudo chsh -s /usr/bin/fish $$(whoami)

flatpak:
	flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
	flatpak install flathub -y io.ente.auth \
	                            dev.geopjr.Tuba \
	                            org.onlyoffice.desktopeditors \
	                            org.gnome.World.PikaBackup \
	                            net.jami.Jami

kubectl:
	curl -LO https://dl.k8s.io/release/v1.32.0/bin/linux/amd64/kubectl
	install -m 755 kubectl $(BIN)/kubectl
	rm -f kubectl

go:
	@set -x; \
	TMP_DIR=$$(mktemp -d); \
	cd $$TMP_DIR && \
	VERSION=$$(curl -s https://go.dev/VERSION?m=text | cut -d' ' -f1 | grep -v time) && \
	curl -LO "https://go.dev/dl/$${VERSION}.linux-amd64.tar.gz" && \
	tar -C $(BIN)/ -xzf "$${VERSION}.linux-amd64.tar.gz" && \
	rm -rf $$TMP_DIR

emacs:
	mkdir -p ~/.emacs.d/ ~/.config/systemd/user/
	ln -sf $(DIR_CONF)/.emacs.d/init.el $(HOME)/.emacs.d/init.el
	ln -sf $(DIR_CONF)/.emacs.d/early-init.el $(HOME)/.emacs.d/early-init.el
	ln -sf $(DIR_CONF)/.emacs.d/elpaca.el $(HOME)/.emacs.d/elpaca.el
	ln -sf $(DIR_CONF)/.config/systemd/user/emacs.service $(HOME)/.config/systemd/user/emacs.service
	systemctl --user enable emacs.service
	systemctl --user start emacs.service

kitty:
	ln -sf $(DIR_CONF)/.config/kitty/kitty.conf ~/.config/kitty/kitty.conf

virt:
	sudo pacman -S --needed --noconfirm qemu-full virt-manager virt-viewer dnsmasq vde2 openbsd-netcat libvirt
	sudo systemctl enable --now libvirtd

clean:
	rm -f $(BIN)/kubectl
	rm -rf $(BIN)/go
	rm -rf ~/.config/fish/{functions/alias.fish,config.fish,completions/k9s.fish}
	rm -f ~/.emacs.d/{init.el,early-init.el,elpaca.el}
	rm -f ~/.config/kitty/kitty.conf
	rm -f ~/.config/systemd/user/emacs.service
	sudo rm -f /etc/pacman.d/hooks/clean-cache.hook
