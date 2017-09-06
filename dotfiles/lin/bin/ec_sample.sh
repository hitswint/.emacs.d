#!/bin/sh

Wind_wine=`xdotool getwindowfocus -f`

if [ $(xdotool search --onlyvisible --class "wine" | grep -i $Wind_wine) ]; then
    word=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
else
    word=$(xclip -selection primary -o | sed 's/[\"]/\\&/g')
fi

run-or-raise.sh emacs
emacsclient -e "(progn (w3m-youdao-sample-sentences (substring-no-properties \"$word\")) (sit-for 60))"
