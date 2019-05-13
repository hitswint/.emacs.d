#!/bin/bash

server=$(hostname)
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")
((port++))

if [ x$1 != x ]; then
    if [[ ! -f $1 ]]; then
        echo "$0: first argument is not a file" >&2
        exit 1
    fi
    cat $1 | nc -l -p $port
else
    ffmpeg -f v4l2 -video_size 1280x800 -i /dev/video0 -c:v libx264 -preset ultrafast -f avi - | nc -l -p $port
fi
