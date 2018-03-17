#!/bin/bash

xwin_id=`xdpyinfo | sed -ne 's/^focus:.*\(0x[^,]\+\).*/\1/p'`
if xwininfo -id $xwin_id | grep "(has no name)"
then
    xwin_id=`printf "0x%x\n" $(( $xwin_id - 1 ))` #Decrement by one.
fi

Wind_id=`xdotool getactivewindow`;
sleep 0.5

if [ $(xdotool search --onlyvisible --class "llpp" | grep -i $Wind_id) ]; then
    word=$(xclip -selection primary -o | sed 's/[\"]/\\&/g')
else
    xdotool key --clearmodifiers ctrl+c
    word=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
fi

run-or-raise.sh emacs

ELISP=$( cat $HOME/bin/emacs_anywhere.el )

emacsclient -a '' -c -e "(progn $ELISP (swint-online-to-buffer (substring-no-properties \"$word\")))"

wmctrl -ia $xwin_id
