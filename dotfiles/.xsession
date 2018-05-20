#!/bin/sh
#
#~/.xinitrc
#
#Executed by startx(run your window manager from here)
#
fcitx

conky | while true; read line; do xsetroot -name "$line"; done &

wicd-client -t &
# dbus-launch nm-applet &

xmodmap ~/.xmodmap

# MPD daemon start (if no other user instance exists)
# [ ! -s ~/.mpd/mpd.pid ] && mpd  # Fail to startup sometime
[ ! -s ~/.mpd/mpd.pid ] ; mpd

dunst &

dwm-multi-mon.sh single

xkeysnail.sh &

pgrep redshift || redshift -l 41.8:123.4 &

exec dwm
