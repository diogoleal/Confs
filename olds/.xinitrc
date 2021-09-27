#!/usr/bin/sh

# Start mpd
mpd ~/.mpd.conf

# Fonts
xset +fp ~/.fonts/
xset fp rehash

# layout keyboard
#setxkbmap -layout us -model "abnt2"
setxkbmap -layout us -variant intl
xmodmap ~/.Xmodmap

# custom X settings
xrdb -merge ~/.Xresources

# set cursor
xsetroot -cursor_name left_ptr
xset r rate 200 20

# start xbanish
xbanish &

# set background
#feh --bg-fill ~/Pictures/Wallpapers/ctrl+esc.png
#feh --bg-fill ~/Pictures/Wallpapers/Classic_Video_Games.jpg

# daemon urxvt
urxvtd --quiet --opendisplay --fork

#use syndaemon to disable touchpad while typing
syndaemon -t -k -i 1 -d
synclient TapButton1=1

# Turns off the terminal bell
#/usr/bin/xset b off
#xset m 1 0 &

#compton -cCf -D2 -e 0.4 -o1 -r8 -l-12 -t-1 &

# brightness
#xbacklight -set 30

exec i3
