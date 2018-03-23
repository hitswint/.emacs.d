#! /bin/bash

Windx=`xdotool search --onlyvisible --classname $1 | head -1`
if [ $Windx ]; then
    xdotool windowactivate --sync $Windx;
else
    $2;
fi
