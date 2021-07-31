#!/bin/bash

# * 使用xrandr速度较慢
# XRANDR=$(which xrandr)
# MONITORS=( $( ${XRANDR} | awk '( $2 == "connected" ){ print $1 }' ) )
# NUM_MONITORS=${#MONITORS[@]}
# # NUM_MONITORS=($(${XRANDR} --listmonitors | grep Monitors: | awk '{print $2}'))

# * 使用python中Gdk模块
NUM_MONITORS=$(python3 -c 'import gi; gi.require_version("Gdk", "3.0"); from gi.repository import Gdk; display=Gdk.Display.get_default(); print(display.get_n_monitors())')

if [ $NUM_MONITORS -gt 1 ]; then
    ActiveWindow="$(xdotool getactivewindow)"

    if [ -z $ActiveWindow ]
    then
        urxvt &
        APP_PID=$!
        sleep 0.2
        xdotool mousemove -w $(xdotool getactivewindow) --polar 0 0
        kill $APP_PID
    else
        xdotool mousemove -w $(xdotool getactivewindow) --polar 0 0
    fi
fi

# xdotool模拟按键为keydown+keyup的完整过程，若按键已被用户按下，则无法正确发送按键
# xdotool keyup super+n
# xdotool sleep 0.1 key --clearmodifiers ctrl+super+n

# 取消keynav后台模式
keynav start
