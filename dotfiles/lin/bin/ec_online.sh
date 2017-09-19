#!/bin/sh

Wind_wine=`xdotool getwindowfocus -f`

if [ $(xdotool search --onlyvisible --class "wine" | grep -i $Wind_wine) ]; then
    Wind_id=$Wind_wine;
    word=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
else
    Wind_id=`xdotool getactivewindow`;
    word=$(xclip -selection primary -o | sed 's/[\"]/\\&/g')
fi

run-or-raise.sh emacs
sleep 0.1
emacsclient -e "(progn (swint-online-to-buffer (substring-no-properties \"$word\")) (sit-for 60))"

xdotool windowactivate --sync $Wind_id
