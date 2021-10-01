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

screenshot_file=~/Downloads/ScreenShot_$(date '+%Y%m%d_%H%M%S').png

case $input1 in
    s)
        # sleep 0.2 ; scrot -s ~/Downloads/ScreenShot_$(date '+%Y%m%d_%H%M%S').png
        import $screenshot_file
        ;;
    w)
        # sleep 0.2 ; scrot -bs 'ScreenShot_%Y%m%d_%H%M%S.png' -e 'mv $f ~/Downloads/' # 需手动指定窗口
        sleep 0.2 ; scrot -bu $screenshot_file # 截取当前窗口，若需其他窗口，点击即可
        ;;
    d)
        sleep 0.2 ; scrot $screenshot_file
        ;;
    q)
        break
        ;;
esac

xclip -selection clipboard -t image/png -i < $screenshot_file
