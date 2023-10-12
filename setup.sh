#!/bin/sh

export BIN="$HOME/bin/"
DIR_CONF="$HOME/Workspace/Confs"
TERRAFORM_VERSION=latest
HELM_VERSION="latest"
ASDF_VERSION=v0.13.1
HACK_VERSION=v3.003
KIND_VERSION=v0.20.0
JETBRAINS_TOOLBOX_VERSION=2.0.4.17212

mkdir ~/bin -p && mkdir -p ~/lib || true

echo "install packages via dnf ;)"
sudo dnf update -y
sudo dnf install curl git fzf direnv gnome-tweaks util-linux-user delta-git \
                      moreutils podman fish openssl-libs zlib-devel clang \
                      clang-devel bzip2-devel libffi-devel readline-devel sqlite-devel -y

sudo systemctl enable --now podman.socket

ln -sf "$DIR_CONF"/.gitconfig "$HOME"/.gitconfig
ln -sf "$DIR_CONF"/.curlrc "$HOME"/.curlrc
git clone https://github.com/tmux-plugins/tpm "$HOME"/.tmux/plugins/tpm
git clone https://github.com/jimeh/tmux-themepack.git "$HOME"/.tmux/themepack
ln -sf "$DIR_CONF"/.tmux.conf "$HOME"/.tmux.conf

# Fish shell
ln -sf "$DIR_CONF"/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
ln -sf "$DIR_CONF"/.config/fish/config.fish ~/.config/fish/config.fish
curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
fish -c "fisher install laughedelic/pisces"

# kubectl
curl -sLO "https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl"
chmod +x ./kubectl
mv ./kubectl ${BIN}

# Kind
if [ ! -f "${BIN}"/kind ]; then
    echo "install kind"
    curl -sLo ./kind https://kind.sigs.k8s.io/dl/"$KIND_VERSION"/kind-linux-amd64
    chmod +x ./kind
    mv ./kind "${BIN}"/kind
fi

# Flatpak
flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

for i in org.signal.Signal org.telegram.desktop io.podman_desktop.PodmanDesktop com.brave.Browser com.google.Chrome dev.geopjr.Tuba
do
    flatpak -y --user install $i
done

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
sudo dnf install -y neovim
mkdir -p "$HOME"/.config/nvim && true
ln -s "$DIR_CONF"/.vimrc "$HOME"/.config/nvim/init.vim
curl -sfLo "$HOME"/.local/share/nvim/site/autoload/plug.vim --create-dirs \
https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim +PlugInstall +qall

# asdf plugins
## terraform
asdf plugin-add terraform https://github.com/asdf-community/asdf-hashicorp.git
asdf install terraform $TERRAFORM_VERSION
asdf global terraform $TERRAFORM_VERSION
## helm
asdf plugin-add helm https://github.com/Antiarchitect/asdf-helm.git
asdf install helm $HELM_VERSION
asdf global helm $HELM_VERSION
## awscli 2
asdf plugin-add awscli
asdf install awscli latest:2
asdf global awscli latest
## aws-vault
asdf plugin-add aws-vault https://github.com/karancode/asdf-aws-vault.git
asdf install aws-vault latest
asdf global aws-vault latest
## eksctl
asdf plugin-add eksctl https://github.com/elementalvoid/asdf-eksctl.git
asdf install eksctl latest
asdf global aws-vault latest
## heptio-authenticator-aws
asdf plugin-add heptio-authenticator-aws https://github.com/neerfri/asdf-heptio-authenticator-aws.git
asdf install heptio-authenticator-aws latest
asdf global heptio-authenticator-aws latest

# Jetbrains
wget https://download.jetbrains.com/toolbox/jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}.tar.gz
tar -xvf jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}.tar.gz
jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}/jetbrains-toolbox
rm -rf jetbrains-toolbox-${JETBRAINS_TOOLBOX_VERSION}.tar.gz

# GNOME
gsettings set org.gnome.desktop.interface clock-show-weekday true
gsettings set org.gnome.shell favorite-apps "[
                                              'com.brave.Browser.desktop',
                                              'org.gnome.Nautilus.desktop',
                                              'org.gnome.Boxes.desktop',
                                              'jetbrains-pycharm-ce.desktop',
                                              'com.gexperts.Tilix.desktop',
                                              'virt-manager.desktop']"
