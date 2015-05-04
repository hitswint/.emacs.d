#!/bin/sh
#
#~/.xinitrc
#
#Executed by startx(run your window manager from here)
#
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

fcitx

export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk-amd64/jre
export _JAVA_AWT_WM_NONREPARENTING=1

conky | while true; read line; do xsetroot -name "$line"; done &

wicd-client -t &

exec dwm
