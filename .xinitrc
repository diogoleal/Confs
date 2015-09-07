#!/usr/bin/sh

# agent Copy.com
#~/bin/CopyAgent &

# Start mpd
mpd ~/.mpd.conf

# network wicd
#/usr/bin/wicd-gtk --tray &

# layout keyboard
setxkbmap -layout br -model "abnt2"
xmodmap ~/.Xmodmap

# custom X settings
xrdb -merge ~/.Xresources

# set cursor
xsetroot -cursor_name left_ptr

# daemon urxvt
urxvtd --quiet --opendisplay --fork

#use syndaemon to disable touchpad while typing if it is installed!
syndaemon=`which syndaemon`
if [ ! -z "${syndaemon}" ];then
	$syndaemon -t -k -i 1 -d
fi

synclient TapButton1=1

# Turns off the terminal bell
/usr/bin/xset b off

# brightness
xbacklight -set 30

# start awesome
exec awesome
