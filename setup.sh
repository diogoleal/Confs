#!/bin/sh

BIN="$HOME/bin/"
DIR_CONF="$HOME/Workspace/Confs"
HACK_VERSION=v3.003
JETBRAINS_TOOLBOX_VERSION=2.0.4.17212

mkdir ~/bin -p && mkdir -p ~/lib || true

sudo dnf upgrade -y --refresh
sudo dnf install -y curl neovim git fzf direnv util-linux-user git-delta \
                      moreutils podman fish openssl-libs zlib-devel clang \
                      clang-devel bzip2-devel libffi-devel readline-devel \
                      sqlite-devel virt-manager
sudo dnf remove firefox -y
sudo systemctl enable --now podman.socket

# Configs
ln -sf "$DIR_CONF"/.gitconfig "$HOME"/.gitconfig
ln -sf "$DIR_CONF"/.curlrc "$HOME"/.curlrc
git clone https://github.com/tmux-plugins/tpm "$HOME"/.tmux/plugins/tpm
git clone https://github.com/jimeh/tmux-themepack.git "$HOME"/.tmux/themepack
ln -sf "$DIR_CONF"/.tmux.conf "$HOME"/.tmux.conf
ln -sf "$DIR_CONF"/.config/terminator/ "$HOME"/.config

# Fish shell
mkdir -p ~/.config/fish/{functions,completions}
ln -sf "$DIR_CONF"/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
ln -sf "$DIR_CONF"/.config/fish/config.fish ~/.config/fish/config.fish
ln -sf "$DIR_CONF"/.config/fish/k9s.fish ~/.config/fish/completions/k9s.fish
curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
fish -c "fisher install laughedelic/pisces"
sudo chsh -s /usr/bin/fish "${USER}"

# Flatpak
flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub --user -y net.ankiweb.Anki \
                                  org.telegram.desktop \
                                  io.podman_desktop.PodmanDesktop \
                                  dev.geopjr.Tuba \
                                  io.dbeaver.DBeaverCommunity \
                                  org.gnome.World.PikaBackup \
                                  org.mozilla.firefox \
                                  org.freedesktop.Platform.ffmpeg-full/x86_64/23.08 \
                                  com.helix_editor.Helix

flatpak install --user -y --from https://nightly.gnome.org/repo/appstream/org.gnome.Prompt.Devel.flatpakref

# Font Hack
mkdir -p /home/diogo/.local/share/fonts/
wget https://github.com/source-foundry/Hack/releases/download/$HACK_VERSION/Hack-$HACK_VERSION-ttf.zip
unzip Hack-$HACK_VERSION-ttf.zip && mv ttf/* ~/.local/share/fonts/
rmdir ttf
rm Hack-$HACK_VERSION-ttf.zip

# neovim
mkdir -p "$HOME"/.config/nvim && true
ln -s "$DIR_CONF"/.vimrc "$HOME"/.config/nvim/init.vim
curl -sfLo "$HOME"/.local/share/nvim/site/autoload/plug.vim --create-dirs \
https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim +PlugInstall +qall

# Pipewrire
wget https://github.com/werman/noise-suppression-for-voice/releases/download/v1.03/linux-rnnoise.zip
unzip linux-rnnoise.zip
mv linux-rnnoise ~/lib
ln -sf "$DIR_CONF"/.config/pipewire/pipewire.conf.d/99-input-denoising.conf ~/.config/pipewire/pipewire.conf.d/99-input-denoising.conf
systemctl restart --user pipewire.service
rm -rf linux-rnnoise*

# sdkman
curl -s "https://get.sdkman.io" | bash

# Jetbrains
wget https://download.jetbrains.com/toolbox/jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}.tar.gz
tar -xvf jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}.tar.gz
jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}/jetbrains-toolbox
rm -rf jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}*
