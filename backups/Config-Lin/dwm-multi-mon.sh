#! /bin/bash
xrandr --output LVDS1 --auto --right-of VGA1
xrandr --output VGA1 --auto --left-of LVDS1
# xrandr --output VGA1 --auto --same-as LVDS1 --mode 1024x768
