#!/bin/sh

DIR_CONF="$HOME/Workspace/Confs"
GO_VERSION=1.22.5
mkdir ~/bin -p && mkdir -p ~/lib || true

# Configs
ln -sf "$DIR_CONF"/.gitconfig "$HOME"/.gitconfig

# Fish shell
mkdir -p ~/.config/fish/{functions,completions}
ln -sf "$DIR_CONF"/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
ln -sf "$DIR_CONF"/.config/fish/config.fish ~/.config/fish/config.fish
ln -sf "$DIR_CONF"/.config/fish/k9s.fish ~/.config/fish/completions/k9s.fish
curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
fish -c "fisher install laughedelic/pisces"
sudo chsh -s /usr/bin/fish "${USER}"

# Flatpak
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install flathub -y  org.telegram.desktop \
                            io.dbeaver.DBeaverCommunity \
                            org.freedesktop.Platform.ffmpeg-full/x86_64/23.08 \
                            org.kde.tokodon \
                            org.kde.neochat \
                            com.vivaldi.Vivaldi \
                            ch.protonmail.protonmail-bridge \
                            io.ente.auth \
                            org.kde.kleopatra \
                            org.mozilla.Thunderbird \
                            dev.zed.Zed \

sudo dnf install -y direnv fzf fish gh neovim ansible opentofu bat duf procs ripgrep fd-find lazygit

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
wget https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz
tar -zxf go${GO_VERSION}.linux-amd64.tar.gz -C ~/bin
rm -rf go${GO_VERSION}.linux-amd64.tar.gz
