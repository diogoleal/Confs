direnv hook fish | source
fish_add_path /home/diogo/bin/
fish_add_path /var/lib/snapd/snap/bin
set --universal GOPATH /home/diogo/go
function fleet
    /home/diogo/.local/share/JetBrains/Toolbox/scripts/fleet
end

function pycharm
    /home/diogo/.local/share/JetBrains/Toolbox/scripts/pycharm
end

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
