direnv hook fish | source

function fleet
    /home/diogo/.local/share/JetBrains/Toolbox/scripts/fleet
end

function pycharm
    /home/diogo/.local/share/JetBrains/Toolbox/scripts/pycharm
end

function _
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

# pyenv init
#if command -v pyenv 1>/dev/null 2>&1
#  pyenv init - | source
#end
