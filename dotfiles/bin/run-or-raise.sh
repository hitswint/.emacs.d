#! /bin/bash

# 当窗口不在当前monitor的上层时，xdotool/wmctrl都无法在不同monitor间切换。
Windx=`xdotool search --onlyvisible --classname $1 | head -1`
if [ $Windx ]; then
    if [ $1 == "emacs" ]; then
        xdotool mousemove 0 0 click 1 windowactivate --sync $Windx mousemove restore;
    fi
    xdotool windowactivate --sync $Windx;
else
    $2;
fi

# Windx=`wmctrl -l | grep -i $1@`
# if [ "$Windx" ]; then
#     wmctrl -x -a "$1";
# else
#     $2;
# fi
