#!/bin/sh

xwin_id=`xdpyinfo | sed -ne 's/^focus:.*\(0x[^,]\+\).*/\1/p'`
if xwininfo -id $xwin_id | grep "(has no name)"
then
    xwin_id=`printf "0x%x\n" $(( $xwin_id - 1 ))` #Decrement by one.
fi
xwin_title=`xwininfo -id $xwin_id | sed -ne 's/xwininfo: .*"\([^"]\+\)"/\1/p'`

xte_paste()
{
    case $xwin_title in
        *Mozilla\ Firefox* | *Microsoft\ Word* )
            xte <<EOF
sleep 1
keydown Control_L
key y
keyup Control_L
EOF
            ;;
        *)
            xte <<EOF
sleep 1
keydown Control_L
key v
keyup Control_L
EOF
            ;;
    esac
}

run-or-raise.sh emacs
emacsclient -e "(let ((helm-full-frame t)) (save-window-excursion (delete-other-windows) (helm-show-kill-ring)))"

wmctrl -ia $xwin_id
xte_paste
