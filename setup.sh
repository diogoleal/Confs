#!/bin/sh

BIN="$HOME/bin/"
DIR_CONF="$HOME/Workspace/Confs"
ASDF_VERSION=v0.13.1
HACK_VERSION=v3.003
KIND_VERSION=v0.20.0
JETBRAINS_TOOLBOX_VERSION=2.0.4.17212

mkdir ~/bin -p && mkdir -p ~/lib || true

echo "install packages via dnf ;)"
sudo dnf upgrade -y --refresh
sudo dnf install -y curl neovim git fzf direnv util-linux-user delta-git \
                      moreutils podman fish openssl-libs zlib-devel clang \
                      clang-devel bzip2-devel libffi-devel readline-devel sqlite-devel terminator \
                      speech-dispatcher

sudo systemctl enable --now podman.socket

# Configs
ln -sf "$DIR_CONF"/.gitconfig "$HOME"/.gitconfig
ln -sf "$DIR_CONF"/.curlrc "$HOME"/.curlrc
git clone https://github.com/tmux-plugins/tpm "$HOME"/.tmux/plugins/tpm
git clone https://github.com/jimeh/tmux-themepack.git "$HOME"/.tmux/themepack
ln -sf "$DIR_CONF"/.tmux.conf "$HOME"/.tmux.conf
ln -sf "$DIR_CONF"/.config/terminator/ "$HOME"/.config

# Fish shell
ln -sf "$DIR_CONF"/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
ln -sf "$DIR_CONF"/.config/fish/config.fish ~/.config/fish/config.fish
curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
fish -c "fisher install laughedelic/pisces"
sudo chsh -s /usr/bin/fish "${USER}"

# Kind
if [ ! -f "${BIN}"/kind ]; then
    echo "install kind"
    curl -sLo ./kind https://kind.sigs.k8s.io/dl/"$KIND_VERSION"/kind-linux-amd64
    chmod +x ./kind
    mv ./kind "${BIN}"/kind
fi

# Flatpak
flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub --user -y org.signal.Signal \
                                  net.ankiweb.Anki \
                                  org.telegram.desktop \
                                  io.podman_desktop.PodmanDesktop  \
                                  dev.geopjr.Tuba \
                                  io.dbeaver.DBeaverCommunity \
                                  org.gnome.World.PikaBackup

# Font Hack
mkdir -p /home/diogo/.local/share/fonts/
wget https://github.com/source-foundry/Hack/releases/download/$HACK_VERSION/Hack-$HACK_VERSION-ttf.zip
unzip Hack-$HACK_VERSION-ttf.zip && mv ttf/* ~/.local/share/fonts/
rmdir ttf
rm Hack-$HACK_VERSION-ttf.zip

# Pipewrire
wget https://github.com/werman/noise-suppression-for-voice/releases/download/v1.03/linux-rnnoise.zip
unzip linux-rnnoise.zip
mv linux-rnnoise ~/lib
ln -sf "$DIR_CONF"/.config/pipewire/pipewire.conf.d/99-input-denoising.conf ~/.config/pipewire/pipewire.conf.d/99-input-denoising.conf
systemctl restart --user pipewire.service
rm linux-rnnoise.zip

# sdkman
curl -s "https://get.sdkman.io" | bash

# asdf
if [ ! -d "${HOME}"/.asdf ]; then
    echo "install asdf"
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch ${ASDF_VERSION}
    source ~/.asdf/asdf.fish
    mkdir -p ~/.config/fish/completions; and ln -s ~/.asdf/completions/asdf.fish ~/.config/fish/completions
fi

# neovim
mkdir -p "$HOME"/.config/nvim && true
ln -s "$DIR_CONF"/.vimrc "$HOME"/.config/nvim/init.vim
curl -sfLo "$HOME"/.local/share/nvim/site/autoload/plug.vim --create-dirs \
https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim +PlugInstall +qall

# asdf plugins
asdfPlugins=(
  "terraform"
  "helm"
  "aws-vault"
  "eksctl"
  "heptio-authenticator-aws"
  "kubectl")

asdfPluginsAdd=(
  "https://github.com/asdf-community/asdf-hashicorp.git"
  "https://github.com/Antiarchitect/asdf-helm.git"
  "https://github.com/karancode/asdf-aws-vault.git"
  "https://github.com/elementalvoid/asdf-eksctl.git"
  "https://github.com/neerfri/asdf-heptio-authenticator-aws.git"
  "https://github.com/asdf-community/asdf-kubectl.git")

VolIndex=0
MaxIndices=${#asdfPlugins[@]}

while (($VolIndex < $MaxIndices))
do
    asdf plugin-add "${asdfPlugins[$VolIndex]} ${asdfPluginsAdd[$VolIndex]}"
    asdf install "${asdfPlugins[$VolIndex]}" latest
    asdf global "${asdfPlugins[$VolIndex]}" latest
    ((++VolIndex))
done

# Jetbrains
wget https://download.jetbrains.com/toolbox/jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}.tar.gz
tar -xvf jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}.tar.gz
jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}/jetbrains-toolbox
rm -rf jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}.tar.gz

# GNOME

gsettings set org.gnome.software download-updates false
gsettings set org.gnome.software download-updates-notify false
gsettings set org.gnome.software first-run false

gsettings set org.gnome.desktop.input-sources xkb-options "['caps:swapescape']"
gsettings set org.gnome.desktop.peripherals.touchpad disable-while-typing true

gsettings set org.gnome.desktop.interface clock-show-weekday true
gsettings set org.gnome.shell favorite-apps "[
                                              'firefox.desktop',
                                              'org.gnome.Nautilus.desktop',
                                              'org.gnome.Boxes.desktop',
                                              'jetbrains-pycharm-ce.desktop',
                                              'terminator.desktop',
                                              'virt-manager.desktop']"
