#!/bin/bash

case $1 in
    down)
        brightnessctl set 5%-
        ;;
    up)
        brightnessctl set 5%+
        ;;
    off)
        sleep 1 && xset -display :0.0 dpms force off
        ;;
    q)
        break
        ;;
esac
