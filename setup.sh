#!/bin/sh

export BIN="$HOME/bin/"
ZSH_DIR="$HOME/.zsh"
TERRAFORM_VERSION="latest"
HELM_VERSION="latest"
ASDF_VERSION=v0.9.0
TERRAGRUNT_VERSION=0.35.4
TERRAFORM_DOCS_VERSION=0.16.0
DIR_CONF="$HOME/Workspace/Confs"

mkdir ~/bin -p && mkdir -p ~/lib || true

__f_config(){
    ln -sf $DIR_CONF/.gitconfig ~/.gitconfig
    ln -sf $DIR_CONF/.curlrc ~/.curlrc
}

__f_tmux(){
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
    ln -sf $DIR_CONF/.tmux.conf ~/.tmux.conf
}

__f_vim(){
    sudo dnf install -y neovim
    mkdir -p ~/.vim && true
    mkdir -p ~/.config/nvim && true
    ln -s $DIR_CONF/.vimrc ~/.vimrc
    ln -s $DIR_CONF/.vimrc ~/.config/nvim/init.vim
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
         https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    vim +PlugInstall +qall
}

__f_espanso(){
    wget -O ~/bin/Espanso.AppImage 'https://github.com/federico-terzi/espanso/releases/download/v2.1.8/Espanso-X11.AppImage'
    chmod u+x ~/bin/Espanso.AppImage
    sudo ~/bin/Espanso.AppImage env-path register
    echo "Register espanso as a systemd service"
    espanso service register
    echo "Start espanso"
    espanso start
}

__f_python(){
    # install to flycheck
    pip install --upgrade pip
    sudo dnf install openssl-libs zlib-devel clang clang-devel bzip2-devel libffi-devel readline-devel sqlite-devel -y
    # python3 -m pip install --user pipx
    # pyenv
    curl https://pyenv.run | bash
    mkdir ~/.pyenv/cache/
    pip install pylint
    pip install git+https://github.com/psf/black
}

__f_fish(){
    sudo dnf install -y fish
    ln -sf $DIR_CONF/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
    ln -sf $DIR_CONF/.config/fish/config.fish ~/.config/fish/config.fish
    curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    exec fish
    fisher install laughedelic/pisces
}

__f_pipewrire(){
    wget https://github.com/werman/noise-suppression-for-voice/releases/download/v1.03/linux-rnnoise.zip
    unzip linux-rnnoise.zip
    mv linux-rnnoise ~/lib
    ln -sf $DIR_CONF/.config/pipewire/pipewire.conf.d/99-input-denoising.conf ~/.config/pipewire/pipewire.conf.d/99-input-denoising.conf
    systemctl restart --user pipewire.service
    rm linux-rnnoise.zip
}

__f_kind(){
    if [ ! -f ${HOME}/bin/kind ]; then
        echo "install kind"
        curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.11.1/kind-linux-amd64
        chmod +x ./kind
        mv ./kind ${HOME}/bin/kind
        rm kind
    fi
}

__f_asdf(){
    if [ ! -d ${HOME}/.asdf ]; then
        echo "install asdf"
        git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch ${ASDF_VERSION}
    fi
}

__f_asdf_plugins(){
    echo "add plugin Terraform"
    asdf plugin-add terraform https://github.com/asdf-community/asdf-hashicorp.git
    asdf install terraform $TERRAFORM_VERSION
    asdf global terraform $TERRAFORM_VERSION
    echo "add plugin helm"
    asdf plugin-add helm https://github.com/Antiarchitect/asdf-helm.git
    asdf install helm $HELM_VERSION
    asdf global helm $HELM_VERSION
    echo "add Terragrunt"
    asdf plugin add terragrunt
    asdf install terragrunt $TERRAGRUNT_VERSION
    asdf global terragrunt $TERRAGRUNT_VERSION
    echo "add terraform-docs"
    asdf plugin-add terraform-docs https://github.com/looztra/asdf-terraform-docs
    asdf install terraform-docs $TERRAFORM_DOCS_VERSION
    asdf global terraform-docs $TERRAFORM_DOCS_VERSION
    echo "add awscli 2"
    asdf plugin add awscli
    asdf install awscli latest:2
    asdf global awscli latest
    echo "add aws-vault"
    asdf plugin-add aws-vault https://github.com/karancode/asdf-aws-vault.git
    asdf install aws-vault latest
    asdf global aws-vault latest
}

__f_install_tools(){
    echo "install fzf, direnv, Docker, Discord, Telegram, Font Hack... ;)"
    unzip Hack-v3.003-ttf.zip && mv ttf/* ~/.local/share/fonts/
    rmdir ttf
    rm Hack-v3.003-ttf.zip

    sudo dnf install fzf direnv gnome-tweaks util-linux-user delta-git -y
    __f_install_flatpak

    for i in org.signal.Signal com.discordapp.Discord org.telegram.desktop org.mozilla.firefox com.spotify.Client com.github.micahflee.torbrowser-launcher io.podman_desktop.PodmanDesktop
    do
        flatpak -y --user install $i
    done
    __f_install_sublime
    __f_kubectl

    curl -s "https://get.sdkman.io" | bash
}

__f_install_podman(){
    sudo dnf install podman -y
    sudo systemctl enable --now podman.socket
}

__f_install_flatpak(){
    flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
}

__f_install_sublime(){
    sudo rpm -v --import https://download.sublimetext.com/sublimehq-rpm-pub.gpg
    sudo dnf config-manager --add-repo https://download.sublimetext.com/rpm/stable/x86_64/sublime-text.repo
    sudo dnf install sublime-text -y
}

__f_kubectl(){
    PATH_KUBECTL=/usr/local/bin/kubectl

    if [ ! -e "$PATH_KUBECTL" ]; then
        curl -LO "https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl"
    fi
    if [ ! -x "$PATH_KUBECTL" ]; then
        chmod +x ./kubectl
        sudo mv ./kubectl ${PATH_KUBECTL}
        kubectl version --client
    fi
}

__f_emacs() {
    wget https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip
    ln -sf $DIR_CONF/.emacs ~/.emacs
    mkdir -p ~/.config/systemd/user/
    cp .config/systemd/user/emacs.service ~/.config/systemd/user/emacs.service
    systemctl enable --user emacs
    systemctl start --user emacs
}

__f_zsh(){
    sudo dnf install zsh -y
    mkdir -p  ~/.zsh/plugins ~/.zsh/themes
    git clone git@github.com:spaceship-prompt/spaceship-prompt.git ~/.zsh/themes/spaceship-prompt
    git clone git@github.com:zdharma-zmirror/fast-syntax-highlighting.git ~/.zsh/plugins/fast-syntax-highlighting
    git clone git@github.com:zsh-users/zsh-autosuggestions.git ~/.zsh/plugins/zsh-autosuggestions
    git clone git@github.com:zsh-users/zsh-completions.git ~/.zsh/plugins/zsh-completions
    if [ ! -f ${HOME}/.zshrc ]; then
        #        cp -v .zshrc .zshenv ~/
        ln -sf $DIR_CONF/.zshrc ~/.zshrc
    fi

    # chsh -s /bin/zsh "${USER}"
    # . "${HOME}"/.zshrc
}

option="${1}"
case "${option}" in
  "vim")
      __f_vim
      ;;
  "fish")
      __f_fish
      ;;
  "zsh")
    __f_zsh
    ;;
  "config")
    __f_config
    ;;
  "tmux")
      __f_tmux
      ;;
  "python")
      __f_python
      ;;
  "k8s")
      __f_kind
      __f_kubectl
      ;;
  "asdf")
      __f_asdf
      __f_asdf_plugins
      ;;
  "pipewrire")
      __f_pipewrire
      ;;
  "espanso")
      __f_espanso
      ;;
  "all")
      __f_install_tools
      __f_pipewrire
      __f_zsh
      __f_tmux
      __f_emacs
      __f_kind
      __f_kubectl
      __f_install_podman
      __f_espanso
      ;;
  *)
      echo " setup.sh
        zsh => I love it
        emacs => my main editor
        tmux => install tmux, tpm, themes and configuration
        python => my main programming language
        config => curl and gitconfig
        k8s => install kind and kubectl
        asdf => install asdf and plugins
        pipewrire => configure noise suppression for voice
        podman => install podman

        ### I don't use more ###
        sublime => install sublime
        vim => install vim and plugins (I don't use more)
        fish => install fish (I don't use more)

        ### install all ###
        all my active settings"
        exit 1
       ;;
esac
