#!/usr/bin/sh

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
xset r rate 200 20

# start xbanish
xbanish &

# set background
#feh --bg-fill ~/Pictures/Wallpapers/ctrl+esc.png
feh --bg-fill ~/Pictures/Wallpapers/Classic_Video_Games.jpg

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
xbacklight -set 30

# panel for snapwm
#while true; do xsetroot -name "\
#    wifi: $(iwconfig  wlp2s0 |grep ESSID |awk '{print $4,$5}') \
#    |$(battery -p /sys/class/power_supply/BAT1/uevent) % \
#    |$(date +%H:%M:%S) " ;sleep 1; done &
#exec uwm
#exec snapwm
exec herbstluftwm --locked
#exec i3
