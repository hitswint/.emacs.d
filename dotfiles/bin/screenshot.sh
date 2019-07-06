#!/bin/bash

if [ x$1 == x ]; then
    echo "Choose:"
    echo "s) Select"
    echo "w) Window"
    echo "d) Desktop"
    read input1
else
    input1=$1
fi

case $input1 in
    s)
        # scrot -s
        import ~/Downloads/ScreenShot_$(date '+%Y-%m-%d_at_%H:%M:%S').png
        ;;
    w)
        sleep 0.2 ; scrot -bs 'ScreenShot_%Y-%m-%d_at_%H:%M:%S.png' -e 'mv $f ~/Downloads/'
        ;;
    d)
        sleep 0.2 ; scrot 'ScreenShot_%Y-%m-%d_at_%H:%M:%S.png' -e 'mv $f ~/Downloads/'
        ;;
    q)
        break
        ;;
esac
