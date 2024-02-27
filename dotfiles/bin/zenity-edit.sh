#!/bin/bash

Wind_id=`xdotool getactivewindow`;
sleep 0.5

if [ $(xdotool search --onlyvisible --class "URxvt" | grep -i $Wind_id) ]; then
    selectText=$(xclip -selection primary -o)
else
    if [ $(xdotool search --onlyvisible --class "Emacs" | grep -i $Wind_id) ]; then
        xdotool keyup alt+w
        xdotool key --clearmodifiers alt+w
        sleep 1
    elif [ $(xdotool search --onlyvisible --class "qpdfview" | grep -i $Wind_id) ]; then
        echo None
    else
        xdotool keyup ctrl+c
        xdotool key --clearmodifiers ctrl+c
    fi
    selectText=$(xclip -selection clipboard -o)
fi

export transEdit=$HOME/.zenity_edit

selectText=$(echo $selectText | sed 's/[\"]/\\&/g' | tr -d '\n')
echo "$selectText" > $transEdit

zenity --width=800 --height=500 --text-info --title "Edit" --editable --filename=$transEdit | xclip -selection clipboard -i

sleep 0.2

window_class=`xprop -id $(xprop -root _NET_ACTIVE_WINDOW | cut -d ' ' -f 5) | grep WM_CLASS | awk '{print $4}'`
URxvt_style='^.*(URxvt|tabbed|llpp).*$'

if [[ $window_class =~ $URxvt_style ]]; then
    xdotool keyup shift+Insert
    xdotool key shift+Insert
else
    xdotool keyup ctrl+v
    xdotool key --clearmodifiers ctrl+v
fi

# sleep 0.1
# xdotool keyup Return
# xdotool key Return
