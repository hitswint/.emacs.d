#!/bin/sh
#
#~/.xinitrc
#
#Executed by startx(run your window manager from here)
#

fcitx &

conky | while true; read line; do xsetroot -name "$line"; done &

dunst &

# wicd-client -t &
nm-applet &
# dbus-launch nm-applet &
blueman-applet &

# MPD daemon start (if no other user instance exists)
# [ ! -s ~/.mpd/mpd.pid ] && mpd  # Fail to startup sometime
[ ! -s ~/.mpd/mpd.pid ] ; mpd &

# dwm-multi-mon.sh single

# # * Home
# # ** eDP-1
# xrandr --output eDP-1 --off
# # xrandr --output eDP-1 --auto --primary
# # ** HDMI
# # *** 默认1920x1080
xrandr --output HDMI-1 --auto --primary
# # *** 60.00刷新率，T510显卡不支持。
# # cvt 2560 1440 60                # 将输出加到newmode后
# # xrandr --newmode "2560x1440_60.00" 312.25 2560 2752 3024 3488 1440 1443 1448 1493 -hsync +vsync
# # xrandr --addmode HDMI-1 "2560x1440_60.00"
# # xrandr --output HDMI-1 --mode "2560x1440_60.00"
# # *** 44.00刷新率
# # cvt 2560 1440 44                # 将输出加到newmode后
# xrandr --newmode "2560x1440_44.00" 222.75 2560 2720 2992 3424 1440 1443 1448 1479 -hsync +vsync
# xrandr --addmode HDMI-1 "2560x1440_44.00"
# xrandr --output HDMI-1 --mode "2560x1440_44.00" --primary
# # ** DP-2
# xrandr --newmode "1920x1080_60.00" 173.00 1920 2048 2248 2576 1080 1083 1088 1120 -hsync +vsync
# xrandr --addmode DP-2 "1920x1080_60.00"
# xrandr --output DP-2 --mode "1920x1080_60.00" --right-of HDMI-1

# * Work
# # ** LVDS-1
# xrandr --output LVDS-1 --auto --primary
# # ** VGA
# # xrandr --newmode "1920x1080_60.00" 173.00 1920 2048 2248 2576 1080 1083 1088 1120 -hsync +vsync
# # xrandr --addmode VGA-1 "1920x1080_60.00"
# # xrandr --output VGA-1 --mode "1920x1080_60.00" --right-of LVDS-1
xrandr --output VGA-1 --auto --right-of HDMI-1

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
