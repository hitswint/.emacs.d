#!/bin/sh

Wind_wine=`xdotool getwindowfocus -f`
sleep 0.1

if [ $(xdotool search --onlyvisible --class "wine" | grep -i $Wind_wine) ]; then
    xdotool key --clearmodifiers ctrl+c
    Wind_id=$Wind_wine;
    word=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
else
    Wind_id=`xdotool getactivewindow`;
    word=$(xclip -selection primary -o | sed 's/[\"]/\\&/g')
fi

run-or-raise.sh emacs

ELISP=$( cat $HOME/bin/emacs_anywhere.el )

emacsclient -a '' -c -e "(progn $ELISP (swint-online-to-buffer (substring-no-properties \"$word\")))"

xdotool windowactivate --sync $Wind_id && exit 0
