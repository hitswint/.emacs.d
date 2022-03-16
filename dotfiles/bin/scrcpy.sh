#! /bin/bash

xhost + local:docker && sudo docker run --rm -i -t --privileged -v /dev/bus/usb:/dev/bus/usb -v /tmp/.X11-unix:/tmp/.X11-unix -v /home:/home -e DISPLAY=$DISPLAY pierlo1/scrcpy:nvidia sh -c "scrcpy --bit-rate 2M --max-size 800 --max-fps 10 --shortcut-mod=lctrl --push-target /sdcard/download"
