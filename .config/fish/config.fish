/usr/bin/direnv hook fish | source
fish_add_path /home/diogo/bin/
fish_add_path /var/lib/snapd/snap/bin
fish_add_path /home/diogo/.cargo/bin
fish_add_path /home/diogo/.local/bin
fish_add_path /home/diogo/bin/go/bin

set --universal GOPATH /home/diogo/Workspace/go
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
