#!/usr/bin/sh

# pidgin ofline
#/usr/bin/pidgin --nologin  &

# agent Copy.com
~/bin/CopyAgent &

# network wicd
#/usr/bin/wicd-gtk --tray &

# network-manager
/usr/bin/nm-applet &

# daemon urxvt
urxvtd --quiet --opendisplay --fork

# layout keyboard
setxkbmap -layout br -model "abnt2"

# custom X settings
xrdb -merge ~/.Xresources

/usr/bin/xmodmap ~/.Xmodmap

# set cursor
xsetroot -cursor_name left_ptr

#use syndaemon to disable touchpad while typing if it is installed!
syndaemon=`which syndaemon`
if [ ! -z "${syndaemon}" ];then
	$syndaemon -t -k -i 1 -d
fi

synclient TapButton1=1

# Turns off the terminal bell
/usr/bin/xset b off

# Volume
/usr/bin/volumeicon &

# set wallpaper
#feh --bg-fill /home/diogo/Pictures/Wallpapers/limbo_basic_lines_wallpaper_by_arand4-d5ozxbx.jpg

exec awesome
