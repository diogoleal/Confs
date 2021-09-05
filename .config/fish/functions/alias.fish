
function tchau
  sudo poweroff
end

function v
  vim $argv
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

