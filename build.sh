#!/bin/sh

export BIN="$HOME/bin/"
ZSH_DIR="$HOME/.zsh"
TERRAFORM_VERSION=1.1
HELM_VERSION="latest"
ASDF_VERSION=v0.9.0
TERRAGRUNT_VERSION=0.35.4
TERRAFORM_DOCS_VERSION=0.16.0
DIR_CONF="$HOME/Workspace/Confs"

__f_createbin(){
  mkdir ~/bin
}

__f_config(){
  cp -rf .config/htop .gitconfig ~/
  # cp -rf .Xmodmap .Xresources .xinitrc .rtorrent.rc .config/htop/ .Xresources.d ~/
}

__f_tmux(){
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
  git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux/themepack
  ln -s $DIR_CONF/.tmux.conf ~/.tmux.conf
}

__f_vim(){
  mkdir -p ~/.vim && true
  mkdir -p ~/.config/nvim && true
  cp -v .vimrc ~/
  ln -s ~/.vimrc ~/.config/nvim/init.vim
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  vim +PlugInstall +qall
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
  # curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  fisher install laughedelic/pisces
  python3 -m pip install --user pipx
  register-python-argcomplete --shell fish pipx >~/.config/fish/completions/pipx.fish
  pipx install virtualfish
  fish_add_path ~/.local/bin
  # exec fish
  vf install compat_aliases projects environment
  exec fish
}

__f_kind(){
  if [ ! -f ${HOME}/bin/kind ]; then
    echo "install kind"
    curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.11.1/kind-linux-amd64
    chmod +x ./kind
    mv ./kind ${HOME}/bin/kind
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
}

__f_install_tools(){
  echo "install fzf, direnv, neovim, Docker, Discord, Telegram ... ;)"
  sudo dnf install fzf direnv neovim gnome-tweaks util-linux-user -y
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
    ln -s ~/Workspace/Confs/.emacs ~/.emacs
    mkdir -p ~/.config/systemd/user/
    cp .config/systemd/user/emacs.service ~/.config/systemd/user/emacs.service
    systemctl enable --user emacs
    systemctl start --user emacs
}

__f_zsh(){
  #mkdir -p "${ZSH_DIR}"
  sudo dnf install zsh -y

  if [ ! -f ${HOME}/.zshrc ] && [ ! -f ${HOME}/.zshenv ]; then
    cp -v .zshrc .zshenv ~/
  fi

  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

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
  "tmux")
    __f_tmux
    ;;
  "python")
    __f_python
    ;;
  "all")
    __f_zsh
    # __f_kind
    # __f_kubectl
    __f_install_tools
    __f_install_podman
     ;;
   *)
     echo " build.sh vim | fish | zsh | all | emacs | tmux |python"
     exit 1
    ;;
esac
