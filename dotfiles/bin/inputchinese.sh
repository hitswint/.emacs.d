#! /bin/bash

zenity --text "输入汉字" --entry | xclip -selection clipboard -i

sleep 0.2

window_class=`xprop -id $(xprop -root _NET_ACTIVE_WINDOW | cut -d ' ' -f 5) | grep WM_CLASS | awk '{print $4}'`
URxvt_style='^.*(URxvt|tabbed|llpp).*$'

if [[ $window_class =~ $URxvt_style ]]; then
    xdotool key shift+Insert
else
    xdotool key --clearmodifiers ctrl+v
fi

# sleep 0.1
# xdotool key Return
