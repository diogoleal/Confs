#!/bin/bash

IN="LVDS1"
EXT="HDMI1"

if (xrandr | grep "$EXT disconnected"); then
    xrandr --output $IN --auto --output $EXT --off
else
    xrandr --output $IN --auto --primary --output $EXT --auto --above $IN
fi
