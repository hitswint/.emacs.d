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
blueman-applet &

# MPD daemon start (if no other user instance exists)
# [ ! -s ~/.mpd/mpd.pid ] && mpd  # Fail to startup sometime
[ ! -s ~/.mpd/mpd.pid ] ; mpd

dunst &

dwm-multi-mon.sh single

xkeysnail.sh &

# keynav &

pgrep redshift || redshift -l 41.8:123.4 &

# sleep 10 && xmodmap ~/.xmodmap &
# Swap Escape and grave
# xmodmap -e " keycode 49 = Escape NoSymbol Escape "
# xmodmap -e " keycode 9 = grave asciitilde grave asciitilde "

# pulseaudio -D
pgrep pasystray || (pgrep pulseaudio && pasystray -i 5 || sleep 10 ; pasystray -i 5) &

# greenclip daemon &
copyq &

exec dwm
