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
        # sleep 0.2 ; scrot -s ~/Downloads/ScreenShot_$(date '+%Y%m%d_%H%M%S').png
        import ~/Downloads/ScreenShot_$(date '+%Y%m%d_%H%M%S').png
        ;;
    w)
        # sleep 0.2 ; scrot -bs 'ScreenShot_%Y%m%d_%H%M%S.png' -e 'mv $f ~/Downloads/' # 需手动指定窗口
        sleep 0.2 ; scrot -bu 'ScreenShot_%Y%m%d_%H%M%S.png' -e 'mv $f ~/Downloads/' # 截取当前窗口，若需其他窗口，点击即可
        ;;
    d)
        sleep 0.2 ; scrot 'ScreenShot_%Y%m%d_%H%M%S.png' -e 'mv $f ~/Downloads/'
        ;;
    q)
        break
        ;;
esac
