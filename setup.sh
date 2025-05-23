#!/bin/sh

DIR_CONF="$HOME/Workspace/Confs"
mkdir ~/{bin,lib} -p || true
mkdir ~/Workspace || true

sudo dnf remove firefox -y
sudo dnf upgrade -y --refresh
sudo rpm -v --import https://download.sublimetext.com/sublimehq-rpm-pub.gpg
sudo dnf config-manager addrepo --from-repofile=https://download.sublimetext.com/rpm/stable/x86_64/sublime-text.repo
sudo dnf install -y direnv fzf fish gh ansible opentofu bat duf procs ripgrep \
         fd-find lazygit btop yt-dlp+default sublime-text emacs

# Configs
ln -sf "$DIR_CONF"/.gitconfig "$HOME"/.gitconfig

# Fish shell
mkdir -p ~/.config/fish/{functions,completions}
ln -sf "$DIR_CONF"/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
ln -sf "$DIR_CONF"/.config/fish/config.fish ~/.config/fish/config.fish
ln -sf "$DIR_CONF"/.config/fish/k9s.fish ~/.config/fish/completions/k9s.fish
curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
sudo chsh -s /usr/bin/fish "${USER}"

# Flatpak
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install flathub -y  org.telegram.desktop \
                            com.vivaldi.Vivaldi \
                            io.ente.auth \
                            net.cozic.joplin_desktop \
                            org.gnome.Meld \
                            org.onlyoffice.desktopeditors
# eksctl
curl -sLO "https://github.com/eksctl-io/eksctl/releases/latest/download/eksctl_Linux_amd64.tar.gz"
tar -xzf eksctl_Linux_amd64.tar.gz -C /tmp && rm eksctl_Linux_amd64.tar.gz
mv /tmp/eksctl ~/bin
mkdir  -p ~/.config/fish/completions
eksctl completion fish > ~/.config/fish/completions/eksctl.fish

# kubectl
curl -LO https://dl.k8s.io/release/v1.30.0/bin/linux/amd64/kubectl
install -m=+x+r kubectl ~/bin/kubectl
rm -rf kubectl

# Go
VERSION=$(curl -s https://go.dev/VERSION?m=text)
curl -LO https://go.dev/dl/${VERSION}.linux-amd64.tar.gz
tar -C ~/bin/ -xzf ${VERSION}.linux-amd64.tar.gz
rm -rf ${VERSION}.linux-amd64.tar.gz

# emacs
ln -sf "$DIR_CONF"/.emacs.d/init.el  ~/.emacs.d/init.el
