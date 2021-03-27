#!/bin/bash

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

# xdotool模拟按键为keydown+keyup的完整过程，若按键已被用户按下，则无法正确发送按键
xdotool keyup ctrl+super+n
xdotool key --clearmodifiers ctrl+super+n
