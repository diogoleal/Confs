#!/bin/sh

DIR_CONF="$HOME/Workspace/Confs"
mkdir ~/{bin,lib} -p || true
mkdir ~/Workspace || true

sudo dnf remove firefox rhythmbox -y
sudo dnf upgrade -y --refresh
sudo dnf install -y direnv fzf fish ansible opentofu bat duf procs ripgrep \
         fd-find btop yt-dlp+default kitty
# emacs dependencies
sudo dnf install -y emacs libtool cmake clang-tools-extra nodejs npm

# Configs
ln -sf "$DIR_CONF"/.gitconfig "$HOME"/.gitconfig

# Fish shell
mkdir -p ~/.config/fish/{functions,completions}
ln -sf "$DIR_CONF"/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
ln -sf "$DIR_CONF"/.config/fish/config.fish ~/.config/fish/config.fish
ln -sf "$DIR_CONF"/.config/fish/k9s.fish ~/.config/fish/completions/k9s.fish
#curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
sudo chsh -s /usr/bin/fish "${USER}"

# Flatpak
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install flathub -y  org.telegram.desktop \
                            com.vivaldi.Vivaldi \
                            io.ente.auth \
                            net.cozic.joplin_desktop \
                            dev.geopjr.Tuba \
                            org.onlyoffice.desktopeditors \
                            com.pikatorrent.PikaTorrent \
                            dev.geopjr.Tuba \
                            org.gnome.World.PikaBackup \
                            net.jami.Jami

# eksctl
#curl -sLO "https://github.com/eksctl-io/eksctl/releases/latest/download/eksctl_Linux_amd64.tar.gz"
#tar -xzf eksctl_Linux_amd64.tar.gz -C /tmp && rm eksctl_Linux_amd64.tar.gz
#mv /tmp/eksctl ~/bin
#mkdir  -p ~/.config/fish/completions
#eksctl completion fish > ~/.config/fish/completions/eksctl.fish

# kubectl
curl -LO https://dl.k8s.io/release/v1.32.0/bin/linux/amd64/kubectl
install -m=+x+r kubectl ~/bin/kubectl
rm -rf kubectl

# Go
VERSION=$(curl -s https://go.dev/VERSION?m=text | cut -d' ' -f1 |grep -v time)
curl -LO https://go.dev/dl/${VERSION}.linux-amd64.tar.gz
tar -C ~/bin/ -xzf ${VERSION}.linux-amd64.tar.gz
rm -rf ${VERSION}.linux-amd64.tar.gz

# emacs
ln -sf "$DIR_CONF"/.emacs.d/init.el  ~/.emacs.d/init.el
go install golang.org/x/tools/gopls@latest
npm install bash-language-server yaml-language-server pyright

# tmux
ln -sf "$DIR_CONF"/.tmux.conf  ~/.tmux.conf

# Kitty
ln -sf "$DIR_CONF"/.config/kitty/kitty.conf ~/.config/kitty/kitty.conf

# filen.io
sudo dnf install -y https://cdn.filen.io/@filen/desktop/release/latest/Filen_linux_x86_64.rpm

# mega
wget https://mega.nz/linux/repo/Fedora_42/x86_64/megasync-Fedora_42.x86_64.rpm
sudo dnf install "$PWD/megasync-Fedora_42.x86_64.rpm"
rm -rf "$PWD/megasync-Fedora_42.x86_64.rpm"
