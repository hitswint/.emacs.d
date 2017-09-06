#!/bin/sh

Wind_wine=`xdotool getwindowfocus -f`

if [ $(xdotool search --onlyvisible --class "wine" | grep -i $Wind_wine) ]; then
    Wind_id=$Wind_wine;
else
    Wind_id=`xdotool getactivewindow`;
fi

run-or-raise.sh emacs
emacsclient -e "(let ((helm-full-frame t)) (save-window-excursion (delete-other-windows) (helm-show-kill-ring)))"

xdotool windowactivate --sync $Wind_id
