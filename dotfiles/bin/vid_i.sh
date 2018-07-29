#!/bin/bash

server=$1
IP=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server" | sed 's/^.*@//g')
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")
((port++))

nc $IP $port | mplayer -fps 31 - -cache 1024
