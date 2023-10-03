#!/bin/sh

export BIN="$HOME/bin/"
TERRAFORM_VERSION="latest"
HELM_VERSION="latest"
ASDF_VERSION=v0.9.0
TERRAGRUNT_VERSION=0.35.4
TERRAFORM_DOCS_VERSION=0.16.0
DIR_CONF="$HOME/Workspace/Confs"
PYENV_ROOT="$HOME/.pyenv"
HACK_VERSION="v3.003"
KIND_VERSION="v0.11.1"

mkdir ~/bin -p && mkdir -p ~/lib || true

__f_config(){
    ln -sf "$DIR_CONF"/.gitconfig "$HOME"/.gitconfig
    ln -sf "$DIR_CONF"/.curlrc "$HOME"/.curlrc
}

__f_tmux(){
    git clone https://github.com/tmux-plugins/tpm "$HOME"/.tmux/plugins/tpm
    git clone https://github.com/jimeh/tmux-themepack.git "$HOME"/.tmux/themepack
    ln -sf "$DIR_CONF"/.tmux.conf "$HOME"/.tmux.conf
}

__f_vim(){
    sudo dnf install -y neovim
    mkdir -p "$HOME"/.vim && true
    mkdir -p "$HOME"/.config/nvim && true
    ln -s "$DIR_CONF"/.vimrc "$HOME"/.vimrc
    ln -s "$DIR_CONF"/.vimrc "$HOME"/.config/nvim/init.vim
    curl -fLo "$HOME"/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    curl -fLo "$HOME"/.vim/autoload/plug.vim --create-dirs \
         https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    vim +PlugInstall +qall
}

__f_espanso(){
    wget -O "$HOME"/bin/Espanso.AppImage 'https://github.com/federico-terzi/espanso/releases/download/v2.1.8/Espanso-X11.AppImage'
    chmod u+x "$HOME"/bin/Espanso.AppImage
    sudo "$HOME"/bin/Espanso.AppImage env-path register
    echo "Register espanso as a systemd service"
    espanso service register
    echo "Start espanso"
    espanso start
}

__f_python(){
    pip install --upgrade pip
    sudo dnf install openssl-libs zlib-devel clang clang-devel bzip2-devel libffi-devel readline-devel sqlite-devel -y
    # python3 -m pip install --user pipx
    pip install pylint
    pip install git+https://github.com/psf/black
}

__f_pyenv(){
    __f_fish
    git clone https://github.com/pyenv/pyenv.git "$PYENV_ROOT"
    echo "set --export PYENV_ROOT $HOME/.pyenv" > ~/.config/fish/conf.d/pyenv.fish
    git clone https://github.com/pyenv/pyenv-virtualenv.git "$PYENV_ROOT"/plugins/pyenv-virtualenv
    ln -sf "$DIR_CONF"/.config/fish/conf.d/pyenv.fish ~/.config/fish/conf.d/pyenv.fish
}

__f_fish(){
    sudo dnf install -y fish
    ln -sf "$DIR_CONF"/.config/fish/functions/alias.fish ~/.config/fish/functions/alias.fish
    ln -sf "$DIR_CONF"/.config/fish/config.fish ~/.config/fish/config.fish
    curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    fish -c "fisher install laughedelic/pisces"
}

__f_pipewrire(){
    wget https://github.com/werman/noise-suppression-for-voice/releases/download/v1.03/linux-rnnoise.zip
    unzip linux-rnnoise.zip
    mv linux-rnnoise ~/lib
    ln -sf "$DIR_CONF"/.config/pipewire/pipewire.conf.d/99-input-denoising.conf ~/.config/pipewire/pipewire.conf.d/99-input-denoising.conf
    systemctl restart --user pipewire.service
    rm linux-rnnoise.zip
}

__f_kind(){
    if [ ! -f "${HOME}"/bin/kind ]; then
        echo "install kind"
        curl -Lo ./kind https://kind.sigs.k8s.io/dl/"$KIND_VERSION"/kind-linux-amd64
        chmod +x ./kind
        mv ./kind "${HOME}"/bin/kind
        rm kind
    fi
}

__f_asdf(){
    if [ ! -d "${HOME}"/.asdf ]; then
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


__f_install_hack_fonts(){
    wget https://github.com/source-foundry/Hack/releases/download/$HACK_VERSION/Hack-$HACK_VERSION-ttf.zip
    unzip Hack-$HACK_VERSION-ttf.zip && mv ttf/* ~/.local/share/fonts/
    rmdir ttf
    rm Hack-$HACK_VERSION-ttf.zip
}

__f_install_tools(){
    echo "install fzf, direnv, Docker, Discord, Telegram, Font Hack... ;)"
    __f_install_hack_fonts
    sudo dnf install fzf direnv gnome-tweaks util-linux-user delta-git moreutils -y
    __f_install_flatpak

    for i in org.signal.Signal com.discordapp.Discord org.telegram.desktop org.mozilla.firefox com io.podman_desktop.PodmanDesktop
    do
        flatpak -y --user install $i
    done
    # __f_install_sublime
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

__f_emacs(){
    __f_install_hack_fonts
    ln -sf "$DIR_CONF"/.emacs "$HOME"/.emacs
    mkdir -p "$HOME"/.config/systemd/user/
    cp .config/systemd/user/emacs.service "$HOME"/.config/systemd/user/emacs.service
    systemctl enable --user emacs
    systemctl start --user emacs
}

__f_zsh(){
    sudo dnf install zsh -y
    mkdir -p  "$HOME"/.zsh/plugins ~/.zsh/themes
    git clone git@github.com:spaceship-prompt/spaceship-prompt.git "$HOME"/.zsh/themes/spaceship-prompt
    git clone git@github.com:zdharma-zmirror/fast-syntax-highlighting.git "$HOME"/.zsh/plugins/fast-syntax-highlighting
    git clone git@github.com:zsh-users/zsh-autosuggestions.git "$HOME"/.zsh/plugins/zsh-autosuggestions
    git clone git@github.com:zsh-users/zsh-completions.git "$HOME"/.zsh/plugins/zsh-completions
    if [ ! -f "${HOME}"/.zshrc ]; then
        #        cp -v .zshrc .zshenv ~/
        ln -sf "$DIR_CONF"/.zshrc "$HOME"/.zshrc
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
      __f_fish
      __f_tmux
      __f_emacs
      __f_kind
      __f_kubectl
      __f_install_podman
      __f_espanso
      ;;
  *)
      echo " setup.sh
        fish => my favorite shell
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
        zsh => install zsh (I don't use more)

        ### install all ###
        all => my active settings"
        exit 1
       ;;
esac
