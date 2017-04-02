
export PAGER="less -R"
export EDITOR=vim
export TERM=xterm
#LS_COLORS="so=35:tw=91:ow=93"

# GoLang Path
export GOPATH=$HOME/Workspace/go
export PATH=$PATH:$GOPATH/bin

export PATH="$PATH:$HOME/bin"

# Pyenv
export PATH="/home/diogo/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export WORKON_HOME=~/.ve
export PROJECT_HOME=~/Workspace
eval "$(pyenv init -)"

#rbenv
#~/.rbenv/bin/rbenv init
export PATH="$HOME/.rbenv/bin:$PATH"

PATH="/home/diogo/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/diogo/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/diogo/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/diogo/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/diogo/perl5"; export PERL_MM_OPT;

#export PATH="$PATH:$HOME/.rvm/bin"
#[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

