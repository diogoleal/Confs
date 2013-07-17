#!/usr/bin/sh

/usr/bin/terminator &
#/usr/bin/xscreensaver -no-splash &
#/usr/bin/conky &
/usr/bin/pidgin &
/usr/bin/tint2 &
/usr/bin/owncloud &
/usr/bin/emacs --daemon 

# layout keyboard 
setxkbmap -layout br -model "abnt2"

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
#sysresources=/etc/X11/xinit/.Xresources
#sysmodmap=/etc/X11/xinit/.Xmodmap
#
# merge in defaults and keymaps
#[ -f $sysresources ] && /usr/bin/xrdb -merge $sysresources
#[ -f $sysmodmap ] && /usr/bin/xmodmap $sysmodmap
#[ -f $userresources ] && /usr/bin/xrdb -merge $userresources
#[ -f $usermodmap ] && /usr/bin/xmodmap $usermodmap

exec openbox-session
#exec i3
#exec gnome-session
#exec startlxde

