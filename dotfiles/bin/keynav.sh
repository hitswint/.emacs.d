#!/bin/bash

# xdotool模拟按键为keydown+keyup的完整过程，若按键已被用户按下，则无法正确发送按键
# xdotool keyup super+n
# xdotool sleep 0.1 key --clearmodifiers ctrl+super+n

# 取消keynav后台模式
keynav start
