#!/usr/bin/sh

# agent Copy.com
#~/bin/CopyAgent &

# Start mpd
mpd ~/.mpd.conf

# Fonts
xset +fp ~/.fonts/
xset fp rehash

# layout keyboard
setxkbmap -layout br -model "abnt2"
xmodmap ~/.Xmodmap

# custom X settings
xrdb -merge ~/.Xresources

# set cursor
xsetroot -cursor_name left_ptr

# start xbanish
xbanish &

# Define background
feh --bg-center ~/Pictures/cave-story_00404484.jpg

# daemon urxvt
urxvtd --quiet --opendisplay --fork

#use syndaemon to disable touchpad while typing
syndaemon -t -k -i 1 -d
synclient TapButton1=1


# SSH Agent
eval "$(ssh-agent -s)"

# Turns off the terminal bell
#/usr/bin/xset b off
#xset m 1 0 &

# brightness
xbacklight -set 30

exec i3
