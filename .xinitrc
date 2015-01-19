#!/usr/bin/sh

# pidgin ofline
/usr/bin/pidgin --nologin  &

# agent Copy.com
~/bin/CopyAgent &

# network wicd
/usr/bin/wicd-gtk --tray &

# daemon urxvt
urxvtd --quiet --opendisplay --fork

# layout keyboard
setxkbmap -layout br -model "abnt2"
# custom X settings
xrdb -merge ~/.Xresources

/usr/bin/xmodmap ~/.Xmodmap

# set cursor
xsetroot -cursor_name left_ptr

# Volume
/usr/bin/volumeicon &

# set wallpaper
feh --bg-fill /home/diogo/Pictures/Wallpapers/wallpaper-2306118.jpg

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

exec /usr/bin/qtile
