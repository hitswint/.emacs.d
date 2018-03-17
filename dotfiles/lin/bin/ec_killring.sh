#!/bin/bash

xwin_id=`xdpyinfo | sed -ne 's/^focus:.*\(0x[^,]\+\).*/\1/p'`
if xwininfo -id $xwin_id | grep "(has no name)"
then
    xwin_id=`printf "0x%x\n" $(( $xwin_id - 1 ))` #Decrement by one.
fi
xwin_title=`xwininfo -id $xwin_id | sed -ne 's/xwininfo: .*"\([^"]\+\)"/\1/p'`

Emacs_style='^.*(Mozilla Firefox|Microsoft Word)$'

run-or-raise.sh emacs

trap exit_hook EXIT
exit_hook()
{
    wmctrl -ia $xwin_id

    if [[ $xwin_title =~ $Emacs_style  ]];then
        xdotool key --clearmodifiers ctrl+y
    else
        xdotool key --clearmodifiers ctrl+v
    fi
}

emacsclient -e "(let ((helm-full-frame t)) (save-window-excursion (delete-other-windows) (helm-show-kill-ring)))"
