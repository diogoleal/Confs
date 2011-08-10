#!/bin/sh

/usr/bin/terminator  &
#/usr/bin/xscreensaver -no-splash &
/usr/bin/conky &
/usr/bin/pidgin &
/usr/bin/tint2 &
#/usr/bin/skype &
/usr/lib/SpiderOak/SpiderOak &

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
[ -f $sysresources ] && /usr/bin/xrdb -merge $sysresources
[ -f $sysmodmap ] && /usr/bin/xmodmap $sysmodmap
[ -f $userresources ] && /usr/bin/xrdb -merge $userresources
[ -f $usermodmap ] && /usr/bin/xmodmap $usermodmap

#exec dwm
#if [ -z "$DESKTOP_SESSION" -a -x /usr/bin/ck-launch-session ]; then
#  exec ck-launch-session openbox-session
#else
  exec openbox-session
#fi
