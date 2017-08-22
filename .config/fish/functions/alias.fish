
## xbps / Void Linux
function xq
  sudo xbps-query -Rs $argv
end
#alias xq='sudo xbps-query -Rs'
#alias xl='sudo xbps-query -l'

function xi
  sudo xbps-install $argv
end

function xu
  sudo xbps-install -Su
end

function xr
  sudo xbps-remove $argv
end

#alias xo='sudo xbps-query -O'
#alias xS='sudo xbps-query -RS'
#alias xor='sudo xbps-query -O | sudo xargs xbps-remove -y'

## shutdown
function tchau
  sudo poweroff
end

function v
  vim $argv
end

#alias n='ncmpcpp'
#alias t='task'
#alias p='pass'
function z
  zathura $argv
end

function f
  feh $argv
end

#alias am='alsamixer'

function _
  sudo $argv
end

function tempo
  curl -4 http://wttr.in/RiodeJaneiro
end

#alias -g M='|more'
#alias -g H='|head'
#alias -g T='|tail'

# Set up aliases
#alias mv='nocorrect mv'       # no spelling correction on mv
#alias cp='nocorrect cp'       # no spelling correction on cp
#alias mkdir='nocorrect mkdir' # no spelling correction on mkdir
#alias j=jobs
#alias pu=pushd
#alias po=popd
#alias d='dirs -v'
#alias h=history
#alias grep=egrep
#alias ll='ls -l'
#alias la='ls -a'

# List only directories and symbolic
# links that point to directories
#alias lsd='ls -ld *(-/DN)'

#ls
#alias ls='ls --color'
#alias la='ls --color -a'

# git
#alias g='git'
#alias ga='git add'
#alias glum='git pull upstream master'
#alias gb='git branch'
#alias gcmsg='git commit -m'
#alias gcam='git commit -a -m'
#alias gco='git checkout'
#alias gl='git pull'
#alias gp='git push'

function myip
  curl httpbin.org/ip
end

# sprunge
#alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
