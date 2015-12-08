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

# set background
feh --bg-fill ~/Pictures/Wallpapers/ctrl+esc.png

# daemon urxvt
urxvtd --quiet --opendisplay --fork

#use syndaemon to disable touchpad while typing
syndaemon -t -k -i 1 -d
synclient TapButton1=1

# Turns off the terminal bell
#/usr/bin/xset b off
#xset m 1 0 &

compton -cCf -D2 -e 0.4 -o1 -r8 -l-12 -t-1 &

# brightness
xbacklight -set 30

# panel for snapwm
while true; do xsetroot -name "\
    wifi:  $(iwconfig wlp2s0 | grep 'Bit Rate=' | awk '{gsub(/[=/]/," "); print $3}') Mb/s \
    | $(battery -p /sys/class/power_supply/BAT1/uevent) % \
    | $(date +%H:%M:%S) " ;sleep 1; done &

exec snapwm

#exec bspwm
#exec i3
