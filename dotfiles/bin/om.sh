#!/bin/bash

VERSIONS=$(docker images | awk '($1~/^openmodelica\/openmodelica/) {print $2}')
LAUNCHER="rofi -width 30 -dmenu -i -p OpenModelica"

if [[ -t 0 ]]; then
    om_version=$(echo -e $VERSIONS | percol)
else
    om_version=`echo -e $VERSIONS | $LAUNCHER | awk '{print $1}' | tr -d '\r\n'`
fi

if [[ ${#om_version} -gt 0 ]]; then
    xhost +
    if [[ -t 0 ]]; then
        docker run -it --rm -v "$HOME/OpenModelica:$HOME" -v /tmp/.X11-unix:/tmp/.X11-unix -e "HOME=$HOME" -e "DISPLAY=$DISPLAY" -w "$HOME" --user $UID openmodelica/openmodelica:$om_version OMEdit
    else
        urxvt -e zsh -is eval "docker run -it --rm -v \"$HOME/OpenModelica:$HOME\" -v /tmp/.X11-unix:/tmp/.X11-unix -e \"HOME=$HOME\" -e \"DISPLAY=$DISPLAY\" -w \"$HOME\" --user $UID openmodelica/openmodelica:$om_version OMEdit"
    fi
fi
