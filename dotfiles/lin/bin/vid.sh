#!/bin/bash

server="raspberrypi"
IP=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server" | sed 's/^.*@//g')

nc $IP 10101 | mplayer -fps 31 - -cache 1024
