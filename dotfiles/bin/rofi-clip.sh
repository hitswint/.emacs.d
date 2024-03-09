#!/bin/bash

xwin_id=`xdpyinfo | sed -ne 's/^focus:.*\(0x[^,]\+\).*/\1/p'`
if xwininfo -id $xwin_id | grep "(has no name)"
then
    xwin_id=`printf "0x%x\n" $(( $xwin_id - 1 ))` #Decrement by one.
fi
xwin_title=`xwininfo -id $xwin_id | sed -ne 's/xwininfo: .*"\([^"]\+\)"/\1/p'`

Emacs_style='^.*(emacs@|Microsoft Word).*$'

# * greenclip
# greenclip与emacs/helm存在冲突
# rofi.sh -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}'
# rofi -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}' ; sleep 0.5; xdotool type $(xclip -o -selection clipboard)

if [[ $1 == "copyq" ]]; then
    # * copyq
    copyq toggle                    # 会立刻返回，无法等待后续选择
    while [ $(copyq visible) == "true" ] ; do sleep 0.2; done
else
    # * rofi-copyq
    # 使用rofi.sh中代码，直接将鼠标移动到当前窗口
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

    rofi-copyq
fi

if [[ $xwin_title =~ $Emacs_style  ]]; then
    xdotool keyup ctrl+y
    xdotool key --clearmodifiers ctrl+y
else
    xdotool keyup ctrl+v
    xdotool key --clearmodifiers ctrl+v
fi
