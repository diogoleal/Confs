#!/usr/bin/sh

/usr/bin/emacs --daemon 
/usr/bin/pidgin --nologin  &
/usr/bin/nm-applet &

# layout keyboard 
setxkbmap -layout br -model "abnt2"
/usr/bin/xmodmap ~/.Xmodmap 

exec qtile

