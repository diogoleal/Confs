#!/usr/bin/sh

/usr/bin/vidalia &
#/usr/bin/owncloud &
/usr/bin/emacs --daemon 
/usr/bin/mpd ~/.mpdconf

# layout keyboard 
setxkbmap -layout br -model "abnt2"
xmodmap ~/.Xmodmap 

exec startxfce4

