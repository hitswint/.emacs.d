#!/bin/bash

server=$(hostname)
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")
((port++))

ffmpeg -f v4l2 -video_size 1280x800 -i /dev/video0 -c:v libx264 -preset ultrafast -f avi - | nc -l -p $port
