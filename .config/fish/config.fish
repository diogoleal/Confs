/usr/bin/direnv hook fish | source
fish_add_path /home/diogo/bin/
fish_add_path /var/lib/snapd/snap/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.local/bin
fish_add_path $HOME/bin/go/bin
fish_add_path $HOME/.config/emacs/bin

set --universal GOPATH $HOME/Workspace/go
function S
  sudo $argv
end

function tempo
  curl -4 http://wttr.in/RiodeJaneiro
end

function myip
  curl httpbin.org/ip
end

function sprunge
  curl -F 'sprunge=<-' http://sprunge.us
end

kubectl completion fish | source

set -gx PATH $PATH $HOME/.krew/bin

